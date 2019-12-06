#!/usr/bin/python
# -*- coding: utf-8 -*-
#============================================================================#
#                                Libro                                       #
#     A cross-platform program for statistical analysis of texts             #
#   using Shannon-Weaver information theory and Zipf power law function      #
#   and computation of readability indices (Flesch, Flesch-Kincaid, SMOG,    #
#      Gunning-Fog, Coleman-Liau, and Automated Readability Index)           #
#                                                                            #
#              Copyright 2013-2015 Mauro J. Cavalcanti                       #
#                        maurobio@gmail.com                                  #
#                                                                            #
#   This program is free software: you can redistribute it and/or modify     #
#   it under the terms of the GNU General Public License as published by     #
#   the Free Software Foundation, either version 3 of the License, or        #
#   (at your option) any later version.                                      #
#                                                                            #
#   This program is distributed in the hope that it will be useful,          #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of           #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            #
#   GNU General Public License for more details.                             #
#                                                                            #
#   You should have received a copy of the GNU General Public License        #
#   along with this program. If not, see <http://www.gnu.org/licenses/>.     #
#                                                                            #
#   Requirements:                                                            #
#     Python 2.6+ (www.python.org)                                           #
#     PyQt 4.8+ (www.riverbankcomputing.com/software/pyqt)                   #
#     BeautifulSoup 4.0+ (www.crummy.com/software/BeautifulSoup)             #
#     Matplotlib 0.98+ (www.matplotlib.org)                                  #
#     Odt2txt 0.1+ (www.freewisdom.org/projects/python-markdown/odt2txt.php) #
#                                                                            #
#   REVISION HISTORY:                                                        #
#     Version 1.00, 1st Mar 2013 - Initial release (non-GUI version)         #
#     Varsion 2.00, 1st Mar 2015 - Second release (GUI version)              #
#============================================================================#

from __future__ import division
import sys, os, re, zipfile, shutil, locale, platform
from math import log, sqrt
from string import punctuation
from operator import itemgetter
from PyQt4 import QtCore, QtGui
from PyQt4.QtCore import QT_VERSION_STR
from PyQt4.Qt import PYQT_VERSION_STR
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import bs4
from bs4 import BeautifulSoup

from odt2txt import OpenDocumentTextFile
import resources

__version__ = "2.0.0 (2015-03-10)"

#-- Support functions
def format(num, places = 0):
    return locale.format("%.*f", (places, num), True)

def percent(val1, val2):
    if val2 == 0:
        ret_val = 0.0
    else:
        ret_val = (float(val1) / float(val2)) * 100.00
    return ret_val

def syllable_count(word):
    count = 0
    vowels = 'aeiouy'
    word = word.lower().strip(".:;?!")
    if len(word) == 0:
        count = 0
        return count
    if word[0] in vowels:
        count +=1
    for index in range(1,len(word)):
        if word[index] in vowels and word[index-1] not in vowels:
            count +=1
    if word.endswith('e'):
        count -= 1
    if word.endswith('le'):
        count+=1
    if count == 0:
        count +=1
    return count

class MainWindow(QtGui.QMainWindow):
    
    def __init__(self):
        super(MainWindow, self).__init__()
        self.initUI()
               
    def initUI(self):               
        openAction = QtGui.QAction(QtGui.QIcon(":/open.png"), self.trUtf8("&Open..."), self)
        openAction.setShortcut("Ctrl+O")
        openAction.setStatusTip(self.trUtf8("Open and analyze file"))
        openAction.triggered.connect(self.fileOpenDialog)
        
        exitAction = QtGui.QAction(QtGui.QIcon(":/exit.png"), self.trUtf8("&Exit"), self)        
        exitAction.setShortcut("Ctrl+Q")
        exitAction.setStatusTip(self.trUtf8("Exit application"))
        #exitAction.triggered.connect(QtGui.qApp.quit)
        exitAction.triggered.connect(self.close)
        
        aboutAction = QtGui.QAction(QtGui.QIcon(":/about.png"), self.trUtf8("&About..."), self)
        aboutAction.setStatusTip(self.trUtf8("About this application"))
        aboutAction.triggered.connect(self.helpAboutDialog)
        
        aboutQtAction = QtGui.QAction(QtGui.QIcon(":/qt.png"), self.trUtf8("About &Qt..."), self)
        aboutQtAction.setStatusTip(self.trUtf8("About Qt"))
        aboutQtAction.triggered.connect(self.helpAboutQtDialog)
        
        self.statusBar()
        self.statusBar().showMessage(self.trUtf8("Ready"))

        menubar = self.menuBar()
        fileMenu = menubar.addMenu(self.trUtf8("&File"))
        fileMenu.addAction(openAction)
        fileMenu.addSeparator()
        fileMenu.addAction(exitAction)
        helpMenu = menubar.addMenu(self.trUtf8("&Help"))
        helpMenu.addAction(aboutAction)
        helpMenu.addAction(aboutQtAction)
        
        toolbar = self.addToolBar(self.trUtf8("File"))
        toolbar.addAction(openAction)
        toolbar.addAction(exitAction)
        toolbar.addAction(aboutAction)
        
        self.view = QtGui.QTextBrowser()
        self.setCentralWidget(self.view)
        self.setGeometry(192, 107, 766, 441)
        self.setWindowTitle("Libro")    
        self.setWindowIcon(QtGui.QIcon(":/libro.png"))
        self.setUnifiedTitleAndToolBarOnMac(True)
        self.show()
        
    def fileOpenDialog(self):
        filename = QtGui.QFileDialog.getOpenFileName(self, 
            self.trUtf8("Open file"), 
            "",
            self.trUtf8("Text files (*.txt);;HTML files (*.htm *.html);;EPUB files (*.epub);;ODT files (*.odt)"))
        if not filename.isEmpty():
            os.chdir(str(QtCore.QFileInfo(filename).path()))
            fname = str(QtCore.QFileInfo(filename).fileName())
            QtGui.QApplication.setOverrideCursor(QtGui.QCursor(QtCore.Qt.WaitCursor))
            self.scanAlyze(fname)
            QtGui.QApplication.restoreOverrideCursor()
            self.view.setSource(QtCore.QUrl(("results.htm")))
        
    def helpAboutDialog(self):
        QtGui.QMessageBox.about(self, self.trUtf8("About Libro"), 
            "<b>Libro</b> v" + __version__ + "<p>" + \
            "&copy; 2013-2015 Mauro J. Cavalcanti<p>" + \
            self.trUtf8("This program scans a whole text file (in plain text, HTML, EPUB, or ODT formats) " + \
                "and ranks all used words according to frequency, performing a quantitative analysis of the " +  \
                "text using Shannon-Weaver information statistic and Zipf power law function. It counts " + \
                "words, sentences, chars, spaces, and syllables. Also computes readability indexes " + \
                "(Gunning-Fog, Coleman-Liau, Automated Readability Index (ARI), SMOG grade, " + \
                "Flesch-Kincaid grade level and Flesch Reading Ease)") + "<p>" + \
            "Python: " + platform.python_version() + \
            "<br>Qt: " + QT_VERSION_STR + \
            "<br>PyQt: " +  PYQT_VERSION_STR + \
            "<br>Matplotlib: " + matplotlib.__version__ + \
            "<br>BeautifulSoup: " + bs4.__version__ + \
            "<br>OS: " + platform.system() + ' ' + platform.release() + \
            "<p><a href=""http://librejo.sourceforge.net"">" + self.trUtf8("Libro website") + "</a>"
            )
        
    def helpAboutQtDialog(self):
        QtGui.QMessageBox.aboutQt(self)
        
    def closeEvent(self, event):
        quit_msg = self.trUtf8("Are you sure you want to exit the program?")
        reply = QtGui.QMessageBox.question(self, self.trUtf8("Confirmation"), 
            quit_msg, QtGui.QMessageBox.Yes, QtGui.QMessageBox.No)
        if reply == QtGui.QMessageBox.Yes:
            event.accept()
        else:
            event.ignore()
            
    def scanAlyze(self, filename):
        f = "_temp.txt"
        #-- Text file
        if filename.endswith(".txt"):
            shutil.copyfile(filename, f)
        #-- HTML file
        elif filename.endswith(".htm") or filename.endswith(".html"):
            content = open(filename,"rb").read()
            soup = BeautifulSoup(content)
            text = soup.body.getText().encode("utf-8")
            with open(f, "wb") as outfile:
                outfile.write(text)
        #-- EPUB file
        elif filename.endswith(".epub"):
            lines = []
            zip = zipfile.ZipFile(filename)
            for fname in zip.namelist():
                if fname.endswith(".htm") or fnm.endswith(".html"):
                    content = zip.read(fname)
                    soup = BeautifulSoup(content)
                    text = soup.body.getText().encode("utf-8")
                    lines.append(text)
            zip.close()
            with open(f, "wb") as outfile:
                outfile.writelines("%s\n" % l for l in lines)
        #-- ODT file
        elif filename.endswith(".odt"):
            odt = OpenDocumentTextFile(filename)
            unicode = odt.toString()
            text = unicode.encode("utf-8")
            with open(f, "wb") as outfile:
                outfile.write(text)
        
        #-- Text processing
        totalchars = 0
        num_chars = 0
        no_spaces = 0
        for line in open(f):
            for character in line:
                if character.isalnum():
                    num_chars += 1
                if not character.isspace():
                    no_spaces += 1
                totalchars += 1	
        
        with open(f,"rb") as infile:
            all_text = infile.read()
        sentence_list = re.split(r'[\.\!\?] +(?=[A-Z])', all_text)
        sentences = len(sentence_list)
    
        words = {}
        words_gen = (word.strip(punctuation).lower() for line in open(f)
                                            for word in line.split())
        totalcount = 0
        syllables = 0
        num_syllables = 0
        complex_words = 0
        for word in words_gen:
            words[word] = words.get(word, 0) + 1
            totalcount += 1
            num_syllables = syllable_count(word)
            if num_syllables >= 3:
                complex_words += 1
            syllables += num_syllables

        top_words = sorted(words.iteritems(), key=itemgetter(1), reverse=True)
        count = len(top_words)

        #-- Averages
        wordsPerSentence = totalcount / sentences
        syllablesPerWord = syllables / totalcount
        charsPerWord = no_spaces / totalcount
        
        #-- Create html output file
        basename = os.path.basename(filename)
        outfile = open("Results.htm", "w")
        outfile.write("<!DOCTYPE HTML PUBLIC ""-//W3C//DTD HTML 3.2//EN"">")
        outfile.write("<html>\n")
        outfile.write("<head>\n")
        outfile.write("<title>Text Analysis of " + filename.lower() + "</title>\n")
        outfile.write("</head>\n")
        outfile.write("<body>\n")
        outfile.write("<h2>Text analysis of: " + filename.lower() + "</h2>\n")
        
        #-- Output results
        outfile.write("<table border=1 cellspacing=1 cellpadding=1 width=""80%"">\n")
        outfile.write("<tr><th>Rank</th>")
        outfile.write("<th>Word</th>")
        outfile.write("<th>Frequency</th>")
        outfile.write("<th>%</th>")
        outfile.write("<th>C</th>")
        outfile.write("</tr>\n")
        
        #-- Frequency analysis
        x = []
        y = []
        rank = 0
        h = 0
        for word, frequency in top_words:
            rank += 1
            pi = float(frequency) / float(count)
            h += pi * log(pi,2)
            outfile.write("<tr>")	
            outfile.write("<td align=""Right"">" + str(rank) + "</td>\n")
            outfile.write("<td align=""Center"">" + word + "</td>\n")
            outfile.write("<td align=""Center"">" + str(frequency) + "</td>\n")
            outfile.write("<td align=""Center"">" + format(pi * 100.0, 2) + "</td>\n")
            outfile.write("<td align=""Center"">" + str(rank * frequency) + "</td>\n")
            outfile.write("</tr>")
            x.append(rank)
            y.append(frequency)
        outfile.write("</table>\n")
        
        outfile.write("<h3>Text Statistics</h3>\n")
        outfile.write("<table>\n")
        outfile.write("<tr><td>Number of characters: </th><td>" + str(totalchars) + "</td></tr>\n")
        outfile.write("<tr><td>Number of characters (alphanumeric): </td><td>" + str(num_chars) + "</td></tr>\n")
        outfile.write("<tr><td>Number of characters (without spaces): </td><td>" + str(no_spaces) + "</td></tr>\n")
        outfile.write("<tr><td>Number of words: </td><td>" + str(totalcount) + "</td></tr>\n")
        outfile.write("<tr><td>Number of syllables: </td><td>" + str(syllables) + "</td></tr>\n")
        outfile.write("<tr><td>Number of sentences: </td><td>" + str(sentences) + "</td></tr>\n")
        outfile.write("<tr><td>Average number of characters per word: </td><td>" + format(charsPerWord, 5) + "</td></tr>\n")
        outfile.write("<tr><td>Average number of syllables per word: </td><td>" + format(syllablesPerWord, 5) + "</td></tr>\n")
        outfile.write("<tr><td>Average number of words per sentence: </td><td>" + format(wordsPerSentence, 5) + "</td></tr>\n")
        outfile.write("<tr><td>Different words: </td><td>" + str(count) + "</td></tr>\n")
        outfile.write("<tr><td>% of different words: </td><td>" + format(percent(count, totalcount), 2) + "</td></tr>\n")
        outfile.write("</table>\n")

        #-- Readability indices
        fleschReadability = 206.835 - 1.015 * (totalcount / sentences) - 84.6 * (syllables / totalcount)
        fleschGradeLevel = 0.39 * (totalcount / sentences) + 11.8 * (syllables / totalcount) - 15.59
        colemanLiau = 5.89 * (no_spaces / totalcount) - 29.5 * (sentences / totalcount) - 15.8
        gunningFog = 0.4 * (totalcount / sentences) + 100 * (complex_words / totalcount)
        smog = 1.043 * sqrt(30.0 * (complex_words / sentences)) + 3.1291
        ARI = 4.71 * (num_chars / totalcount) + 0.5 * (totalcount / sentences) - 21.43

        if colemanLiau < 0.0:
            colemanLiau = 0.0
        if ARI < 0.0:
            ARI = 0.0

        outfile.write("<h3>Readability Indices</h3>")
        outfile.write("<table>")
        outfile.write("<tr><td>Flesch Reading Ease = </td><td>" + format(fleschReadability, 5) + "</td></tr>")
        outfile.write("<tr><td>Flesch-Kincaid Grade Level = </td><td>" + format(fleschGradeLevel, 5) + "</td></tr>")
        outfile.write("<tr><td>Gunning-Fog Index = </td><td>" + format(gunningFog, 5) + "</td></tr>")
        outfile.write("<tr><td>SMOG Index = </td><td>" + format(smog, 5) + "</td></tr>")
        outfile.write("<tr><td>Coleman-Liau Index = </td><td>" + format(colemanLiau, 5) + "</td></tr>")
        outfile.write("<tr><td>Automated Readability Index = </td><td>" + format(ARI, 5) + "</td></tr>")
        outfile.write("</table>")

        h *= (-1)
        outfile.write("<h3>Information Statistic</h3>\n")
        outfile.write ("<table>\n")
        outfile.write("<tr><td>Shannon-Wiener H' = </td><td>" + format(h, 5) + "</td></tr>\n")
        outfile.write("</table>\n")
        
        #-- Plot graph
        plt.scatter(x, y)
        plt.loglog(x, y)
        plt.title("Plot of word frequency")
        plt.xlabel("rank (log)")
        plt.ylabel("frequency (log)")
        plt.grid(True)
        figf = "Results.png"
        plt.savefig(figf)
        outfile.write("<img src='" + figf + "'>\n")
        
        outfile.write("</body>\n")
        outfile.write("</html>\n")
        outfile.close()
        if os.path.exists(f):
            os.remove(f)

def main():
    app = QtGui.QApplication(sys.argv)
    locale = QtCore.QLocale.system().name()
    appTranslator = QtCore.QTranslator()
    if appTranslator.load("Libro_" + locale, ":/"):
        app.installTranslator(appTranslator)
    form = MainWindow()
    sys.exit(app.exec_())

if __name__ == '__main__':
    main()