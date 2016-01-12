module Analyse where

import Text.BibTeX.Parse
import Text.BibTeX.Entry
import Text.Parsec.String
import Data.Char
import Data.List

data Type = Article | Conference | TechnicalReport | Book | Thesis | Unknown
    deriving (Show, Eq)

fromEntry :: String -> T -> [String]
fromEntry field entry
    = map (cleanBrackets . snd)
    $ filter ((== field) . lowerCase . fst)
    $ fields entry

yearFromEntry :: T -> [Int]
yearFromEntry = map read . fromEntry "year"

titleFromEntry :: T -> String
titleFromEntry = lowerCase . head . map clean . fromEntry "title"
    where
        clean = filter isAlphaNum

typeFromEntry :: T -> Type
typeFromEntry entry
    | typeName == "conference" || typeName == "inproceedings" = Conference
    | typeName == "incollection" || typeName == "article" || inJournal = Article
    | typeName == "book" = Book
    | typeName == "phdthesis" || typeName == "mastersthesis" = Thesis
    | typeName == "techreport" = TechnicalReport
    | otherwise = Unknown
    where
        typeName = lowerCase $ entryType entry
        inJournal = not . null $ fromEntry "journal" entry

lowerCase :: String -> String
lowerCase = map toLower

cleanBrackets :: String -> String
cleanBrackets ('{':s) = cleanBrackets $ reverse $ tail $ reverse s
cleanBrackets      s  = s

avrg :: [Double] -> Double
avrg l = (sum l) / (fromIntegral $ length l)

unique :: [String] -> [String]
unique = nub

intersections :: String -> [String] -> [String] -> [String] -> IO ()
intersections paper fromGS fromWoS fromSc = do
        putStrLn $ "- " ++ paper ++ ":"
        putStr   $ "   " ++ show g
        putStr   $ " & " ++ show w
        putStr   $ " & " ++ show s

        -- The rest is computed using Inclusion-Exclusion Principle
        putStr   $ " & " ++ show gwI
        putStr   $ " & " ++ show gsI
        putStr   $ " & " ++ show wsI
        putStrLn $ " & " ++ show (gwsU + gwI + gsI + wsI - (g + w + s))
    where
        card = length . unique
        iep2 first second together = (first + second) - together

        -- Unions
        g    = card fromGS
        w    = card fromWoS
        s    = card fromSc
        gwU  = card $ fromGS  ++ fromWoS
        gsU  = card $ fromGS  ++ fromSc
        wsU  = card $ fromWoS ++ fromSc
        gwsU = card $ fromGS  ++ fromWoS ++ fromSc

        -- Intersections
        gwI = iep2 g w gwU
        gsI = iep2 g s gsU
        wsI = iep2 w s wsU

yearsChart :: String -> [Int] -> IO ()
yearsChart name ys = do
        putStr $ "  " ++ name ++ ": "
        print chart
    where
        grouped = groupBy (==) $ sort ys
        chart   = map (\xs -> (head xs, length xs)) grouped

coreForms :: [[Type]] -> IO ()
coreForms typesXX = do
        putStrLn $ intercalate " & " [arts, cnfs, trps, boks, thes, unks, show count]
    where
        types = concat typesXX
        count = length types
        countType t = show $ length $ filter (== t) types

        arts = countType Article
        cnfs = countType Conference
        trps = countType TechnicalReport
        boks = countType Book
        thes = countType Thesis
        unks = countType Unknown

collectData :: String -> IO (Double, [Int], [String], [Type])
collectData name = do
    Right q <- parseFromFile file name

    let years  = concat $ map yearFromEntry q
        yAvrg  = avrg $ map fromIntegral years
        titles = map titleFromEntry q
        types  = map typeFromEntry q

    putStr $ name ++ ": "
    print yAvrg
    return (yAvrg, years, titles, types)

main :: IO ()
main = do
    (c2g, ys2g, t2g, ty2g) <- collectData "bibs/cit2.gs.bib"
    (c2w, ys2w, t2w, ty2w) <- collectData "bibs/cit2.wos.bib"
    (c2s, ys2s, t2s, ty2s) <- collectData "bibs/cit2.sc.bib"

    (c3g, ys3g, t3g, ty3g) <- collectData "bibs/cit3.gs.bib"
    (c3w, ys3w, t3w, ty3w) <- collectData "bibs/cit3.wos.bib"
    (c3s, ys3s, t3s, ty3s) <- collectData "bibs/cit3.sc.bib"

    (c4g, ys4g, t4g, ty4g) <- collectData "bibs/cit4.gs.bib"
    (c4w, ys4w, t4w, ty4w) <- collectData "bibs/cit4.wos.bib"
    (c4s, ys4s, t4s, ty4s) <- collectData "bibs/cit4.sc.bib"

    (c5g, ys5g, t5g, ty5g) <- collectData "bibs/cit5.gs.bib"
    (c5w, ys5w, t5w, ty5w) <- collectData "bibs/cit5.wos.bib"
    (c5s, ys5s, t5s, ty5s) <- collectData "bibs/cit5.sc.bib"

    putStrLn "\nAverage years:"
    putStrLn $ "  GS: "  ++ show (avrg [c2g, c3g, c4g, c5g])
    putStrLn $ "  WoS: " ++ show (avrg [c2w, c3w, c4w, c5w])
    putStrLn $ "  Sc: "  ++ show (avrg [c2s, c3s, c4s, c5s])

    putStrLn "\nYears chart data:"
    yearsChart "GS" $ concat [ys2g, ys3g, ys4g, ys5g]
    yearsChart "WoS"$ concat [ys2w, ys3w, ys4w, ys5w]
    yearsChart "Sc" $ concat [ys2s, ys3s, ys4s, ys5s]

    putStrLn "\nIntersections (GS, WoS, Sc, GS+WoS, GS+Sc, WoS+Sc, GS+WoS+Sc):"
    intersections "Paper 2" t2g t2w t2s
    intersections "Paper 3" t3g t3w t3s
    intersections "Paper 4" t4g t4w t4s
    intersections "Paper 5" t5g t5w t5s

    putStrLn "Core forms of literature (Article, From a Conference, Technical Report, Book, Thesis, Unknown, Total):"
    putStr "  GS: "
    coreForms [ty2g, ty3g, ty4g, ty5g]
    putStr "  WoS: "
    coreForms [ty2w, ty3w, ty4w, ty5w]
    putStr "  Sc: "
    coreForms [ty2s, ty3s, ty4s, ty5s]
