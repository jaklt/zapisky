\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{js}[2012/03/08
  Extended abstract class for Summer School of Algebra]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax

\LoadClass[10pt]{article}
\RequirePackage{amsmath,amssymb,amsthm}
\RequirePackage[top=.9cm,bottom=.6cm,inner=1.5cm,outer=.9cm,a5paper,includefoot]{geometry}
\RequirePackage{enumitem}

\def\@maketitle{\newpage\null
  \begin{center}%
  \let \footnote \thanks
    {\large\scshape \@title \par}%
    \vskip 6pt%
    { \lineskip .5em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
  \end{center}%
  \par
  \vskip .5pc}

\setcounter{secnumdepth}{1}
\renewcommand\section{\@startsection{section}{1}{\the\parindent}%
                                   {-12pt \@plus -1pt \@minus -2pt}%
                                   {3pt \@plus 1pt}%
                                   {\normalfont\large\bfseries}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {-6pt\@plus -1pt \@minus -2pt}%
                                     {2pt}%
                                     {\normalfont\normalsize\bfseries}}
\let\subsubsection\@undefined
\renewcommand\paragraph{\@startsection{paragraph}{3}{\z@}%
                                    {3pt \@minus 1pt}%
                                    {-1em}%
                                    {\normalfont\normalsize\slshape}}
\let\subparagraph\@undefined

\setlist{partopsep=3pt,topsep=3pt,noitemsep}
\setlength{\leftmargini}{3em}
\renewcommand\labelenumi{(\theenumi)}
\renewcommand\labelenumii{(\theenumii)}
\renewcommand\labelenumiii{(\theenumiii)}
\renewcommand\labelenumiv{(\theenumiv)}

\newtheoremstyle{compactplain}{6pt}{6pt}{\slshape}{\z@}{\bfseries}{.}{1em}{}
\newtheoremstyle{compactdefinition}{6pt}{6pt}{}{\z@}{\bfseries}{.}{1em}{}

\theoremstyle{compactplain}
\newtheorem{theorem}{Theorem}
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{proposition}[theorem]{Proposition}
\newtheorem*{corollary}{Corollary}

\theoremstyle{compactdefinition}
\newtheorem{definition}[theorem]{Definition}
\newtheorem*{example}{Example}
