# <img src="python\images\libro.png" alt="Libro" style="zoom:150%;"/> Libro
 A cross-platform program for statistical analysis of texts, using Shannon-Weaver information theory, Zipf power law function, and readability tests

**Libro** is a cross-platform text analysis program written in Python and Free Pascal/Lazarus which scans a whole text file (in plain text, HTML, EPUB, or ODT formats) and ranks all used words according to frequency, performing a quantitative analysis of the text using Shannon-Weaver information statistic and Zipf power law function. It counts words, chars, spaces, and syllables. Also computes readability indexes (Gunning Fog, Coleman-Liau, Automated Readability Index (ARI), SMOG grade, Flesch-Kincaid grade level and Flesch Reading Ease).

**Statistics**

- [Zipf's law](http://en.wikipedia.org/wiki/Zipf_law) states that the frequency of occurence of any word is approximately inversely proportional to its rank in the frequency table. When Zipf's law is applicable, plotting the frequency table on a log-log scale (i.e., log(frequency) versus log(rank order)) will typically show a linear pattern.
- [Shannon-Weaver](http://en.wikipedia.org/wiki/Information_theory) information statistic gives a measure of the entropy (or the average informaton content) of the text, expressed in bits.
- [Gunning Fog](http://en.wikipedia.org/wiki/Gunning-Fog_Index), [Coleman-Liau](http://en.wikipedia.org/wiki/Coleman-Liau_Index), [Automated Readability Index](http://en.wikipedia.org/wiki/Automated_Readability_Index), [SMOG](http://en.wikipedia.org/wiki/SMOG_Index), and [Flesch-Kincaid](http://en.wikipedia.org/wiki/Flesch-Kincaid) readability tests are designed to indicate comprehension difficulty when reading written materials.

## **Remarks**

- Other programs and web sites may give different numerical results for the same text that those computed by **Libro**. This occurs because they may use different formulae, but it is more likely that they use different rules for counting sentences or determining what is a syllable. Indeed, results for the same text may differ in **Libro** itself, if computed from source files in different formats (*eg.* plain text, HTML, EPUB, ODT). However, it is not the exact results themselves which are important, but the qualitative interpretations which may be derived from them, on a comparative basis.
- Support for OpenOffice/LibreOffice format (ODT) is only available in the Python version.

## **Requirements (Python version)**

- [Python](http://www.python.org/) version 2.6 or later 
- [PyQt4](http://www.riverbankcomputing.co.uk/software/pyqt/intro) version 4.8 or later 
- [BeautifulSoup](http://www.crummy.com/software/BeautifulSoup/) version 4.0 or later
- [Matplotlib version 0.98](http://matplotlib.sourceforge.net/) or later

## **Requirements (Free Pascal/Lazarus version)**

- [Free Pascal](http://www.freepascal.org/) version 3.0 or later
- [Lazarus](http://www.lazarus.freepascal.org/) version 1.6 or later 
- [HTMLViewer component](http://wiki.lazarus.freepascal.org/THtmlPort) version 11.8 or later 
- [HistoryFiles component](http://wiki.freepascal.org/HistoryFiles) version 1.3 or later
- [Vector library](http://torry.net/vcl/science/vector/achvectors.zip) version 050702

## **Download**

Source code and binary installaton packages are available from [SourceForge](http://sourceforge.net/projects/librejo/)

## **License**

This program is [free software](https://www.gnu.org/philosophy/free-sw.en.html), made available under the [GNU General Public Licence version 3 (GPL3)](https://www.gnu.org/licenses/gpl.html)

------

© 2013-2019 Mauro J. Cavalcanti, Rio de Janeiro, Brazil



Sponsored by ![rizomaeditorial](C:\Users\mauro\My Documents\Downloads\libro\rizomaeditorial.png)

Powered by ![powered-by](C:\Users\mauro\My Documents\Downloads\libro\powered-by.png)

Powered by ![python](C:\Users\mauro\My Documents\Downloads\libro\python.png) ![pyqt](C:\Users\mauro\My Documents\Downloads\libro\pyqt.png)

