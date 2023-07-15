{==============================================================================}
{                                Libro                                         }
{     A cross-platform program for statistical analysis of texts               }
{   using Shannon-Weaver information theory and Zipf power law function        }
{   and computation of readability indices (Flesch, Flesch-Kincaid, SMOG,      }
{      Gunning-Fog, Coleman-Liau, and Automated Readability Index)             }
{                                                                              }
{              Copyright 2013-2023 Mauro J. Cavalcanti                         }
{                        maurobio@gmail.com                                    }
{                                                                              }
{   This program is free software: you can redistribute it and/or modify       }
{   it under the terms of the GNU General Public License as published by       }
{   the Free Software Foundation, either version 3 of the License, or          }
{   (at your option) any later version.                                        }
{                                                                              }
{   This program is distributed in the hope that it will be useful,            }
{   but WITHOUT ANY WARRANTY; without even the implied warranty of             }
{   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               }
{   GNU General Public License for more details.                               }
{                                                                              }
{   You should have received a copy of the GNU General Public License          }
{   along with this program. If not, see <http://www.gnu.org/licenses/>.       }
{                                                                              }
{   Requirements:                                                              }
{    Lazarus 2.0+ (www.lazarus.freepascal.org)                                 }
{    Free Pascal 3.0+ (www.freepascal.org)                                     }
{    HTMLViewer 11.8+ (wiki.lazarus.freepascal.org/THtmlPort)                  }
{    HistoryFiles 1.3+ (wiki.freepascal.org/HistoryFiles)                      }
{    Vector Library 050702+ (torry.net/files/vcl/science/vector/achvectors.zip)}
{==============================================================================}
unit Main;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, StrUtils, FileUtil, Character,
  TAGraph, TASeries, TATransformations, TASources, PrintersDlgs, Forms,
  Controls, Graphics, Dialogs, ComCtrls, Menus, Clipbrd, IniFiles, HtmlView,
  HistoryFiles, LCLTranslator, Zipper;

type

  { TMainForm }

  TMainForm = class(TForm)
    Chart: TChart;
    CoolBar: TCoolBar;
    CopyBtn: TToolButton;
    EditCopyItem: TMenuItem;
    EditMenu: TMenuItem;
    ExitBtn: TToolButton;
    FileExitItem: TMenuItem;
    FileMenu: TMenuItem;
    FileOpenItem: TMenuItem;
    FilePrintItem: TMenuItem;
    FileSaveItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    HelpBtn: TToolButton;
    HelpMenu: TMenuItem;
    HistoryFiles: THistoryFiles;
    HtmlViewer: THtmlViewer;
    ImageList: TImageList;
    LineSeries: TLineSeries;
    MainMenu: TMainMenu;
    LanguagePortugueseItem: TMenuItem;
    LanguageMenu: TMenuItem;
    LanguageEnglishItem: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    OpenBtn: TToolButton;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    PrintBtn: TToolButton;
    PrintDialog: TPrintDialog;
    SaveBtn: TToolButton;
    SaveDialog: TSaveDialog;
    StatusLine: TStatusBar;
    ResultsTab: TTabSheet;
    PlotTab: TTabSheet;
    ToolBar: TToolBar;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton8: TToolButton;
    XAxisLabels: TListChartSource;
    XAxisLogTransform: TLogarithmAxisTransform;
    XAxisTransformation: TChartAxisTransformations;
    YAxisLabels: TListChartSource;
    YAxisLogTransform: TLogarithmAxisTransform;
    YAxisTransformation: TChartAxisTransformations;
    procedure EditCopyItemClick(Sender: TObject);
    procedure FileExitItemClick(Sender: TObject);
    procedure FileOpenItemClick(Sender: TObject);
    procedure FileSaveItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpAboutItemClick(Sender: TObject);
    procedure HistoryFilesClickHistoryItem(Sender: TObject; Item: TMenuItem;
      const Filename: string);
    procedure LanguageEnglishItemClick(Sender: TObject);
    procedure LanguagePortugueseItemClick(Sender: TObject);
  private
    { private declarations }
    IniFile: TIniFile;
  public
    { public declarations }
    procedure CreateAxisLabels(ASource: TListChartSource; AMin, AMax: double);
    procedure ScanAlyze(fileName: string);
  end;

var
  MainForm: TMainForm;

implementation

uses Aliasv, StrLst, StrCount, VTxtStrm, VectStr, Math, TAChartUtils,
  About;

{$R *.lfm}

resourcestring
  strTitle = 'Text Analysis of ';
  strRank = 'Rank';
  strWord = 'Word';
  strFrequency = 'Frequency';
  strStatText = 'Text Statistics';
  strNumChars = 'Number of characters:';
  strLetterNumberCount = 'Number of characters (alphanumeric):';
  strNonSpaceCount = 'Number of characters (without spaces):';
  strTotalCount = 'Number of words:';
  strDiff = 'Different words:';
  strPercent = '% of different words:';
  strSentences = 'Number of sentences:';
  strSyllables = 'Number of syllables:';
  strWordsPerSentence = 'Average number of words per sentence:';
  strSyllablesPerWord = 'Average number of syllables per word:';
  strCharsPerWord = 'Average number of characters per word:';
  strStatInfo = 'Information Statistic';
  strIndexes = 'Readability Indices';
  strOpenText =
    'Text files (*.txt)|*.txt|HTML files (*.htm *.html)|*.htm;*html|EPUB files (*.epub)|*.epub';
  strSaveText = 'Text files (*.txt)|*.txt|HTML files (*.htm *.html)|*.htm;*html';
  strSaveImage =
    'JPEG files (*.jpg, *.jpeg)|*.jpg;*jpeg|PNG files (*.png)|*.png|Bitmap files (*.bmp)|*.bmp';
  strQuit = 'Do you want to exit the program?';
  strNotFound = 'File not found!';
  strConfirmation = 'Confirmation';
  strError = 'Error';
  strInvalidFile = 'Invalid file type';
  strChartTitle = 'plot of word frequency';
  strLeftTitle = 'frequency (log)';
  strBottomTitle = 'rank (log)';
  strNumLines = 'Number of lines:';
  strBlankLines = 'Number of lines (without blanks):';

function Percent(Val1, Val2: integer): string;
begin
  if (Val2 = 0) then
    Result := ' '
  else
    Result := FloatToStrF((Val1 / Val2) * 100.0, ffFixed, 5, 2);
end;

function Sentences(const string1: string): integer;
var
  I, n_sentences: integer;
begin
  I := 0;
  n_sentences := 0;
  while (I <= Length(string1)) do
  begin
    //if (string1[I] in ['.', '!', '?', ';', ':']) then begin
    if (string1[I] in ['.', '!', '?']) then
    begin
      Inc(n_sentences);
      Inc(I);
    end
    else
      Inc(I);
  end;
  Result := n_sentences;
end;

function StripChars(const aSrc, aCharsToStrip: string): string;
var
  c: char;
begin
  Result := aSrc;
  for c in aCharsToStrip do
    Result := StringReplace(StripChars, c, '', [rfReplaceAll, rfIgnoreCase]);
end;

function Syllables(word: string): integer;
const
  vowels = ['a', 'e', 'i', 'o', 'u', 'y'];
var
  Count, index: integer;
begin
  Count := 0;
  word := LowerCase(StripChars(word, '.:;?!'));
  if (Length(word) = 0) then
  begin
    Count := 0;
    Result := Count;
  end;
  if word[1] in vowels then
    Count += 1;
  for index := 2 to Length(word) do
  begin
    if (word[index] in vowels) and not (word[index - 1] in vowels) then
      Count += 1;
  end;
  if (RightStr(word, 1) = 'e') then
    Count -= 1;
  if (RightStr(word, 2) = 'le') then
    Count += 1;
  if (Count = 0) then
    Count += 1;
  Result := Count;
end;

function StripHTML(S: string): string;
var
  TagBegin, TagEnd, TagLength: integer;
begin
  TagBegin := Pos('<', S);      // search position of first <
  while (TagBegin > 0) do
  begin  // while there is a < in S
    TagEnd := Pos('>', S);              // find the matching >
    TagLength := TagEnd - TagBegin + 1;
    Delete(S, TagBegin, TagLength);     // delete the tag
    TagBegin := Pos('<', S);            // search for next <
  end;
  Result := S;                   // give the result
end;

function ReadFile(const FileName: string): string;
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(FileName);
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

{ TMainForm }

{ Creates the power-of-10 values for the interval AMin..AMax. In order to
  get minor ticks automatically between all labels the first label value is
  smaller than AMin, and the last label value is larger than AMax }
procedure TMainForm.CreateAxisLabels(ASource: TListChartSource; AMin, AMax: double);
var
  x: double;
begin
  ASource.Clear;
  { Round AMin to next-smallest power of 10 }
  x := Power(10.0, Floor(Log10(AMin)));
  while x <= AMax * 10 do
  begin
    { *10 --> make sure to end the axis before the last label }
    { Otherwise the minor ticks would end after the last label drawn }
    ASource.Add(x, x, Format('10^%.0f', [Log10(x)]));
    x := x * 10;
  end;
end;

procedure TMainForm.ScanAlyze(fileName: string);
const
  Delims = [#0..#255] - ['A'..'Z', 'a'..'z'];

var
  TS: TTextStream;
  SC: TStrCounter;
  S: TStrLst;
  Counts: TIntegerVector;
  I, Rank, TotalChars, NumChars, NumLetters, NumSyllables, NumSentences,
  SyllableCount, threeOrMore, NumLines, BlankLines: longint;
  fileExt, fName, txt, St, Lines: string;
  PI, H, Freq, SyllablesPerWord, CharsPerWord, WordsPerSentence,
  fleschReadability, fleschGradeLevel, gunningFog, colemanLiau, smog,
  ARI, XMin, XMax, YMin, YMax: double;
  Ch: char;
  Ex: TDoubleRect;
  Outfile: TextFile;
  UnZipper: TUnZipper;
  FileList: TStringList;
  Files: string;
  Result: boolean;

begin
  Application.ProcessMessages;
  Screen.Cursor := crHourGlass;
  DecimalSeparator := '.';

  fileExt := LowerCase(ExtractFileExt(fileName));
  case fileExt of
    '.txt': fName := fileName;
    '.htm', '.html':
    begin
      fName := 'LIBRO.$$$';
      txt := StripHTML(ReadFile(fileName));
      AssignFile(Outfile, fName);
      Rewrite(Outfile);
      Write(Outfile, txt);
      CloseFile(Outfile);
    end;
    '.epub':
    begin
      fName := 'LIBRO.$$$';
      UnZipper := TUnZipper.Create;
      try
        UnZipper.FileName := fileName;
        UnZipper.OutputPath := '.\tmp';
        UnZipper.UnZipAllFiles;
        FileList := FindAllFiles('.\tmp', '*.htm;*.html', True);
        Files := '';
        for I := 0 to FileList.Count - 1 do
          Files := Files + sLineBreak + FileList[I];
        Result := DeleteDirectory('.\tmp', True);
        if Result then
          Result := RemoveDir('.\tmp');
      finally
        UnZipper.Free;
        FileList.Free;
      end;
      Exit;
    end
    else
    begin
      Application.MessageBox(PChar(strInvalidFile), PChar(strError),
        mb_Ok or mb_IconExclamation);
      Exit;
    end;
  end;

  TS := TTextStream.Create(fName, tsRead);
  SC := TStrCounter.Create;
  S := TStrLst.Create;
  Counts := TIntegerVector.Create(0, 0);
  try
    { Text processing }
    TotalChars := 0;
    NumChars := 0;
    NumLetters := 0;
    NumSyllables := 0;
    NumSentences := 0;
    threeOrMore := 0;
    SyllableCount := 0;
    NumLines := 0;
    BlankLines := 0;
    while not TS.EOF do
    begin
      St := Trim(TS.ReadString);
      for I := 1 to WordCount(St, Delims) do
      begin
        SC.Add(ExtractWord(I, St, Delims));
        S.Add(ExtractWord(I, St, Delims));
      end;
      for Ch in St do
      begin
        if IsLetterOrDigit(Ch) then
          Inc(NumChars);
        if not IsWhiteSpace(Ch) then
          Inc(NumLetters);
        Inc(TotalChars);
      end;
      if (Length(St) > 0) then
      begin
        for I := 1 to WordCount(St, Delims) do
        begin
          SyllableCount := Syllables(ExtractWord(I, St, Delims));
          NumSyllables += SyllableCount;
        end;
        if (SyllableCount >= 3) then
          Inc(threeOrMore);
        NumSentences += Sentences(St);
      end;
      Inc(NumLines);
      if Length(St) = 0 then
        Inc(BlankLines);
    end;

    { Averages }
    SyllablesPerWord := NumSyllables / SC.TotalCount;
    CharsPerWord := NumLetters / SC.TotalCount;
    WordsPerSentence := SC.TotalCount / NumSentences;

    { Readability indices }
    fleschReadability := 206.835 - 1.015 * (SC.TotalCount / NumSentences) -
      84.6 * (NumSyllables / SC.TotalCount);
    fleschGradeLevel := 0.39 * (SC.TotalCount / NumSentences) + 11.8 *
      (NumSyllables / SC.TotalCount) - 15.59;
    colemanLiau := 5.89 * (NumLetters / SC.TotalCount) - 29.5 *
      (NumSentences / SC.TotalCount) - 15.8;
    if (colemanLiau < 0.0) then
      colemanLiau := 0.0;
    gunningFog := 0.4 * (SC.TotalCount / NumSentences) + 100 *
      (threeOrMore / SC.TotalCount);
    smog := 1.043 * Sqrt(30 * (threeOrMore / NumSentences)) + 3.1291;
    ARI := 4.71 * (NumChars / SC.TotalCount) + 0.5 *
      (SC.TotalCount / NumSentences) - 21.43;
    if (ari < 0.0) then
      ari := 0.0;

    { Create html output string }
    Lines := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">' + sLineBreak;
    Lines := Lines + '<html>' + sLineBreak;
    Lines := Lines + '<head>' + sLineBreak;
    Lines := Lines + '<title>Text Analysis of ' + ExtractFileName(fileName) +
      '</title>' + sLineBreak;
    Lines := Lines + '</head>' + sLineBreak;
    Lines := Lines + '<body>' + sLineBreak;
    Lines := Lines + '' + sLineBreak;
    Lines := Lines + '<h2>' + strtitle + ExtractFileName(fileName) +
      '</h2>' + sLineBreak;
    Lines := Lines + '<table border=1 cellspacing=1 cellpadding=1 width="80%">' +
      sLineBreak;
    Lines := Lines + '<tr><th>' + strRank + '</th>' + sLineBreak;
    Lines := Lines + '<th>' + strWord + '</th>' + sLineBreak;
    Lines := Lines + '<th>' + strFrequency + '</th>' + sLineBreak;
    Lines := Lines + '<th>%</th>' + sLineBreak;
    Lines := Lines + '<th>C</th>' + sLineBreak;
    Lines := Lines + '</tr>' + sLineBreak;

    { Frequency analysis }
    S.GetSpectrum(S, Counts);
    Counts.SortDescWith(S);
    XMin := 1E308;
    XMax := 0.0;
    YMin := 1E308;
    YMax := 0.0;
    Rank := 0;
    H := 0.0;
    for I := 0 to S.Count - 1 do
    begin
      Rank := I + 1;
      PI := Counts[I] / SC.Count;
      H := H + PI * Log2(PI);
      Freq := PI;
      LineSeries.AddXY(double(Rank), Freq);
      XMin := Min(XMin, double(Rank));
      XMax := Max(XMax, double(Rank));
      YMin := Min(YMin, Freq);
      YMax := Max(YMax, Freq);
      Lines := Lines + '<tr><td align="Center">' + IntToStr(Rank) + '</td>';
      Lines := Lines + '<td align="Center">' + LowerCase(S[I]) + '</td>';
      Lines := Lines + '<td align="Center">' + IntToStr(Counts[I]) + '</td>';
      Lines := Lines + '<td align="Center">' +
        Percent(Counts[I], SC.Count) + '</td>';
      Lines := Lines + '<td align="Center">' + IntToStr(Rank * Counts[I]) +
        '</td></tr>' + sLineBreak;
    end;
    LineSeries.ListSource.Sorted := True;
    Lines := Lines + '</table>' + sLineBreak;
    Lines := Lines + '' + sLineBreak;
    Lines := Lines + '<h3>' + strStatText + '</h3>' + sLineBreak;
    Lines := Lines + '<table>' + sLineBreak;
    Lines := Lines + Concat('<tr><td>', strNumChars, '</td><td>',
      IntToStr(TotalChars), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>', strLetterNumberCount,
      '</td><td>', IntToStr(NumChars), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>', strNonSpaceCount, '</td><td>',
      IntToStr(NumLetters), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>', strTotalCount, '</td><td>',
      IntToStr(SC.TotalCount), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>', strDiff, '</td><td>',
      IntToStr(SC.Count), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>', strPercent, '</td><td>',
      Percent(SC.Count, SC.TotalCount), '</td><tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>', strSyllables, '</td><td>',
      IntToStr(NumSyllables), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>', strSentences, '</td><td>',
      IntToStr(NumSentences), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>', strCharsPerWord, '</td><td>',
      FloatToStrF(CharsPerWord, ffFixed, 8, 5), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>', strSyllablesPerWord, '</td><td>',
      FloatToStrF(SyllablesPerWord, ffFixed, 8, 5), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>', strWordsPerSentence, '</td><td>',
      FloatToStrF(WordsPerSentence, ffFixed, 8, 5), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>', strNumLines, '</td><td>',
      IntToStr(NumLines), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>', strBlankLines, '</td><td>',
      IntToStr(NumLines - BlankLines), '</td></tr>') + sLineBreak;
    Lines := Lines + '</table>' + sLineBreak;

    H := H * (-1);
    Lines := Lines + '' + sLineBreak;
    Lines := Lines + '<h3>' + strStatInfo + '</h3>' + sLineBreak;
    Lines := Lines + '<table>' + sLineBreak;
    Lines := Lines + Concat('<tr><td>Shannon-Wiener H'' = </td><td>',
      FloatToStrF(H, ffFixed, 7, 5), '</td></tr>') + sLineBreak;
    Lines := Lines + '</table>' + sLineBreak;

    Lines := Lines + '' + sLineBreak;
    Lines := Lines + '<h3>' + strIndexes + '</h3>' + sLineBreak;
    Lines := Lines + '<table>' + sLineBreak;
    Lines := Lines + Concat('<tr><td>Flesch Reading Ease = </td><td>',
      FloatToStrF(fleschReadability, ffFixed, 7, 5), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>Flesch-Kincaid Grade Level = </td><td>',
      FloatToStrF(fleschGradeLevel, ffFixed, 7, 5), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>Gunning-Fog Index = </td><td>',
      FloatToStrF(gunningFog, ffFixed, 7, 5), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>SMOG Index = </td><td>',
      FloatToStrF(smog, ffFixed, 7, 5), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>Coleman-Liau Index = </td><td>',
      FloatToStrF(colemanLiau, ffFixed, 7, 5), '</td></tr>') + sLineBreak;
    Lines := Lines + Concat('<tr><td>Automated Readability Index = </td><td>',
      FloatToStrF(ARI, ffFixed, 7, 5), '</td></tr>') + sLineBreak;
    Lines := Lines + '</table>' + sLineBreak;
    Lines := Lines + '</body>' + sLineBreak;
    Lines := Lines + '</html>';
    HtmlViewer.LoadFromString(Lines);
    HtmlViewer.Visible := True;

    { The Extent of a series contains the minimum and maximum x and y values
    Note: the extent is in "axis" units, i.e. not transformed. }
    Ex := LineSeries.Extent;

    { Calculate x axis labels for the x extent }
    CreateAxisLabels(XAxisLabels, Ex.a.x, Ex.b.x);

    { Calculate y axis labels for the y extent }
    CreateAxisLabels(YAxisLabels, Ex.a.y, Ex.b.y);

    { Use the values in the ChartListsources as axis labels
    This can also be set up in the Object Inspector. }
    Chart.BottomAxis.Marks.Source := XAxisLabels;
    Chart.BottomAxis.Marks.Style := smsLabel;
    Chart.LeftAxis.Marks.Source := YAxislabels;
    Chart.LeftAxis.Marks.Style := smsLabel;

    { Optional: Let axes start at first and end at last label }
    Chart.BottomAxis.Range.Max := XAxisLabels.Item[XAxisLabels.Count - 1]^.X;
    Chart.BottomAxis.Range.UseMax := True;
    Chart.BottomAxis.Range.Min := XAxisLabels.Item[0]^.X;
    Chart.BottomAxis.Range.UseMin := True;
    Chart.LeftAxis.Range.Max := YAxisLabels.Item[YAxisLabels.Count - 1]^.Y;
    Chart.LeftAxis.Range.UseMax := True;
    Chart.LeftAxis.Range.Min := YAxisLabels.Item[0]^.Y;
    Chart.LeftAxis.Range.UseMin := True;
    Chart.LeftAxis.Title.Caption := strLeftTitle;
    Chart.BottomAxis.Title.Caption := strBottomTitle;
    Chart.Title.Text.Clear;
    Chart.Title.Text.Add(strChartTitle);
    Chart.Visible := True;
  finally
    SC.Free;
    TS.Free;
    S.Free;
    Counts.Free;
  end;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.FileOpenItemClick(Sender: TObject);
begin
  OpenDialog.DefaultExt := '.txt';
  OpenDialog.Filter := strOpenText;
  if OpenDialog.Execute then
  begin
    MainForm.Caption := Application.Title + ' - ' + ExtractFileName(OpenDialog.Filename);
    StatusLine.SimpleText := 'Analyzing document - ' + ExtractFileName(OpenDialog.Filename) + '...' ;
    ScanAlyze(OpenDialog.Filename);
    StatusLine.SimpleText := 'Ready';
    HistoryFiles.UpdateList(OpenDialog.Filename);
    PageControl.ActivePage := ResultsTab;
  end;
end;

procedure TMainForm.FileSaveItemClick(Sender: TObject);
var
  Outfile: TextFile;
begin
  SaveDialog.Filename := '';
  if PageControl.ActivePage = ResultsTab then
  begin
    SaveDialog.DefaultExt := '.htm';
    SaveDialog.Filter := strSaveText;
    if SaveDialog.Execute then
    begin
      AssignFile(Outfile, SaveDialog.Filename);
      Rewrite(Outfile);
      case SaveDialog.FilterIndex of
        1: WriteLn(Outfile, StripHTML(HTMLViewer.DocumentSource));
        2: WriteLn(Outfile, HTMLViewer.DocumentSource);
      end;
      CloseFile(Outfile);
    end;
  end
  else
  begin
    SaveDialog.DefaultExt := '.jpg';
    SaveDialog.Filter := strSaveImage;
    if SaveDialog.Execute then
    begin
      case SaveDialog.FilterIndex of
        1: Chart.SaveToFile(TJPEGImage, SaveDialog.Filename);
        2: Chart.SaveToFile(TPortableNetworkGraphic, SaveDialog.Filename);
        3: Chart.SaveToBitmapFile(SaveDialog.Filename);
      end;
    end;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Application.MessageBox(PChar(strQuit), PChar(strConfirmation),
    mb_YesNo or mb_IconQuestion) = mrNo then
    CanClose := False;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  sPath, sLang: string;
begin
  sPath := ExtractFilePath(Application.ExeName);
  HistoryFiles.LocalPath := sPath;
  HistoryFiles.IniFile := sPath + 'Libro.ini';
  HistoryFiles.UpdateParentMenu;
  IniFile := TIniFile.Create(sPath + 'Libro.ini');
  sLang := IniFile.ReadString('Options', 'Language', 'en'); // First default is English
  SetDefaultLang(sLang, 'languages', True);
  PageControl.ActivePage := ResultsTab;
  if sLang = 'en' then
  begin
    LanguageEnglishItem.Checked := True;
    LanguagePortugueseItem.Checked := False;
  end;
  if sLang = 'pt-br' then
  begin
    LanguageEnglishItem.Checked := False;
    LanguagePortugueseItem.Checked := True;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if FileExists('LIBRO.$$$') then
    DeleteFile('LIBRO.$$$');
  IniFile.Free;
end;

procedure TMainForm.HelpAboutItemClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TMainForm.HistoryFilesClickHistoryItem(Sender: TObject;
  Item: TMenuItem; const Filename: string);
begin
  if FileExists(Filename) then
  begin
    MainForm.Caption := Application.Title + ' - ' + ExtractFileName(Filename);
    StatusLine.SimpleText := 'Analyzing document - ' + ExtractFileName(Filename) + '...' ;
    ScanAlyze(Filename);
    StatusLine.SimpleText := 'Ready';
  end
  else
    Application.MessageBox(PChar(strNotFound), PChar(strError), mb_Ok or
      mb_IconExclamation);
end;

procedure TMainForm.LanguageEnglishItemClick(Sender: TObject);
begin
  SetDefaultLang('en', 'language', True);
  IniFile.WriteString('Options', 'Language', 'en');
  LanguageEnglishItem.Checked := True;
  LanguagePortugueseItem.Checked := False;
end;

procedure TMainForm.LanguagePortugueseItemClick(Sender: TObject);
begin
  SetDefaultLang('pt-br', 'language', True);
  IniFile.WriteString('Options', 'Language', 'pt-br');
  LanguageEnglishItem.Checked := False;
  LanguagePortugueseItem.Checked := True;
end;

procedure TMainForm.FileExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.EditCopyItemClick(Sender: TObject);
begin
  if PageControl.ActivePage = ResultsTab then
    Clipboard.AsText := StripHTML(HTMLViewer.DocumentSource)
  else
    Chart.CopyToClipboardBitmap;
end;

end.
