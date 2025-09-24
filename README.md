# <img src="images\libro.png" alt="Libro" style="zoom:150%;"/> Libro
 A cross-platform program for statistical analysis of texts, using Shannon-Weaver information theory, Zipf power law function, and readability tests

**Libro** is a cross-platform text analysis program written in Free Pascal/Lazarus which scans a whole text file (in plain text, HTML, EPUB, or RTF formats) and ranks all used words according to frequency, performing a quantitative analysis of the text using Shannon-Weaver information statistic and Zipf power law function. It counts words, chars, spaces, and syllables. Also computes readability indexes (Gunning Fog, Coleman-Liau, Automated Readability Index (ARI), SMOG grade, Flesch-Kincaid grade level and Flesch Reading Ease).

**Statistics**

- [Zipf's law](http://en.wikipedia.org/wiki/Zipf_law) states that the frequency of occurence of any word is approximately inversely proportional to its rank in the frequency table. When Zipf's law is applicable, plotting the frequency table on a log-log scale (i.e., log(frequency) versus log(rank order)) will typically show a linear pattern.
- [Shannon-Weaver](http://en.wikipedia.org/wiki/Information_theory) information statistic gives a measure of the entropy (or the average informaton content) of the text, expressed in bits.
- [Gunning Fog](http://en.wikipedia.org/wiki/Gunning-Fog_Index), [Coleman-Liau](http://en.wikipedia.org/wiki/Coleman-Liau_Index), [Automated Readability Index](http://en.wikipedia.org/wiki/Automated_Readability_Index), [SMOG](http://en.wikipedia.org/wiki/SMOG_Index), and [Flesch-Kincaid](http://en.wikipedia.org/wiki/Flesch-Kincaid) readability tests are designed to indicate comprehension difficulty when reading written materials.

## **Remarks**

- Other programs and web sites may give different numerical results for the same text that those computed by **Libro**. This occurs because they may use different formulae, but it is more likely that they use different rules for counting sentences or determining what is a syllable. Indeed, results for the same text may differ in **Libro** itself, if computed from source files in different formats (*eg.* plain text, HTML, EPUB, RTF). However, it is not the exact results themselves which are important, but the qualitative interpretations which may be derived from them, on a comparative basis.
- Currently the program interface is available in English, Esperanto, French, German, Italian, Portuguese, Brazilian Portuguese, and Spanish.
- Thanks to wp user from the Lazarus Forum and the ChatGPT and DeepSeek AI's for adding many improvements to the code.

## **Pascal/Lazarus version)**

- [Free Pascal](http://www.freepascal.org/) version 3.0 or later
- [Lazarus](http://www.lazarus.freepascal.org/) version 3.0 or later 

## **Download**

Source code and binary installation packages are available from [SourceForge](http://sourceforge.net/projects/librejo/)

## **License**

This program is [free software](https://www.gnu.org/philosophy/free-sw.en.html), made available under the [GNU General Public Licence version 3 (GPL3)](https://www.gnu.org/licenses/gpl.html)

------

© 2013-2025 Mauro J. Cavalcanti, Rio de Janeiro, Brazil



Sponsored by <img src="images\logo_comciencia.png" alt="Rizoma Editorial"/>ComCiência Editorial

Powered by <img src="images\powered-by.png" alt="Lazarus"/>Lazarus/Free Pascal

