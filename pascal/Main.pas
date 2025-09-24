{==============================================================================}
{                                Libro                                         }
{     A cross-platform program for statistical analysis of texts               }
{   using Shannon-Wiener information theory and Zipf power law function        }
{   and computation of readability indices (Flesch, Flesch-Kincaid, SMOG,      }
{      Gunning-Fog, Coleman-Liau, and Automated Readability Index)             }
{                                                                              }
{              Copyright 2013-2025 Mauro J. Cavalcanti                         }
{                        maurobio@gmail.com                                    }
{                                                                              }
{   This program is free software: you can redistribute it and/or modify       }
{   it under the terms of the GNU General Public License as published by       }
{   the Free Software Foundation, either version 3 of the License, or          }
{   (at your option) any later version.                                        }
{                                                                              }
{   This program is distributed in the hope that it will be useful,            }
{   but WITHOUT ANY WARRANTY; without even the implied warranty of             }
{   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              }
{   GNU General Public License for more details.                               }
{                                                                              }
{   You should have received a copy of the GNU General Public License          }
{   along with this program. If not, see <http://www.gnu.org/licenses/>.       }
{                                                                              }
{   Requirements:                                                              }
{    Lazarus 3.0+ (www.lazarus.freepascal.org)                                 }
{    Free Pascal 3.0+ (www.freepascal.org)                                     }
{                                                                              }
{   Acknowledgements:                                                          }
{    Thanks to wp from the Lazarus Forum, and DeepSeek and ChatGPT5 AIs for    }
{    several improvements to the original code.                                }
{==============================================================================}
unit Main;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, FileUtil,
  TAGraph, TASeries, TATransformations, TASources, Forms,
  Controls, Graphics, Dialogs, ComCtrls, Menus, Clipbrd, IniFiles,
  IpHtml, // !! wp: replaces HtmlView,
  mruManager, //HistoryFiles,   !! wp: replaced by mruManager
  LCLTranslator,
  WordCounter, WordReader;

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
    FileSaveItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    HelpBtn: TToolButton;
    HelpMenu: TMenuItem;
    ImageList: TImageList;
    HtmlPanel: TIpHtmlPanel;
    LineSeries: TLineSeries;
    MainMenu: TMainMenu;
    LanguagePortugueseBrazilianItem: TMenuItem;
    LanguageMenu: TMenuItem;
    LanguageEnglishItem: TMenuItem;
    LanguageSpanishItem: TMenuItem;
    LanguageFrenchItem: TMenuItem;
    LanguagePortugueseItem: TMenuItem;
    LanguageItalianItem: TMenuItem;
    LanguageGermanItem: TMenuItem;
    LanguageEsperantoItem: TMenuItem;
    mnuRecentFiles: TMenuItem;
    N2: TMenuItem;
    OpenBtn: TToolButton;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    RecentFilesPopup: TPopupMenu;
    PrintBtn: TToolButton;
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
    procedure LanguageEnglishItemClick(Sender: TObject);
    procedure LanguagePortugueseBrazilianItemClick(Sender: TObject);
    procedure LanguageEsperantoItemClick(Sender: TObject);
    procedure LanguageFrenchItemClick(Sender: TObject);
    procedure LanguageGermanItemClick(Sender: TObject);
    procedure LanguageItalianItemClick(Sender: TObject);
    procedure LanguagePortugueseItemClick(Sender: TObject);
    procedure LanguageSpanishItemClick(Sender: TObject);
  private
    { private declarations }
    HistoryFiles: TMRUMenuManager; //THistoryFiles;
    IniFile: TIniFile;
    Lines: TStringList;
    CurrentLang: string;
    IsOpen: boolean;
    IsFinished: boolean;
    GlobalWordList: TStringList;
    procedure RecentFileClicked(Sender: TObject; const FileName: string);
    { !! wp: replaced by above
    procedure HistoryFilesClickHistoryItem(Sender: TObject; Item: TMenuItem;
      const Filename: string); }
    procedure UpdateMenu(Sender: TObject);
  public
    { public declarations }
    procedure PlotIt(SC: TWordCounter);
    procedure CreateAxisLabels(ASource: TListChartSource; AMin, AMax: double);
    procedure ScanAnalyze(fileName: string);
  end;

var
  MainForm: TMainForm;

implementation

uses
  Math, TAChartUtils, About, Convert;

  {$R *.lfm}

resourcestring
  strTitle = 'Text Analysis of ';
  strRank = 'Rank';
  strWord = 'Word';
  strFrequency = 'Frequency';
  strStatText = 'Text Statistics';
  strTotalChars = 'Number of characters: ';
  strAlphaNumChars = 'Number of characters (alphanumeric): ';
  strNonSpaceChars = 'Number of characters (without spaces): ';
  strTotalCount = 'Number of words: ';
  strCount = 'Number of unique words: ';
  strPercent = '% of unique words: ';
  strComplexWordCount = 'Number of complex words (>=3 syllables): ';
  strSentenceCount = 'Number of sentences: ';
  strSyllableCount = 'Number of syllables: ';
  strLineCount = 'Number of lines (without blanks): ';
  strBlankLineCount = 'Number of blank lines: ';
  strParagraphs = 'Number of paragraphs: ';
  strWordsPerSentence = 'Average number of words per sentence: ';
  strSyllablesPerWord = 'Average number of syllables per word: ';
  strCharsPerWord = 'Average number of characters per word: ';
  strStatInfo = 'Information Statistic';
  strIndexes = 'Readability Indices';
  strOpenText =
    'Text files (*.txt)|*.txt|HTML files (*.htm *.html)|*.htm;*html|EPUB files (*.epub)|*.epub';
  strSaveText =
    'Text files (*.txt)|*.txt|HTML files (*.htm *.html)|*.htm;*html|CSV files (*.csv)|*.csv';
  strSaveImage =
    'JPEG files (*.jpg, *.jpeg)|*.jpg;*jpeg|PNG files (*.png)|*.png|Bitmap files (*.bmp)|*.bmp';
  strQuit = 'Do you want to exit the program?';
  strNotFound = 'File not found!';
  strConfirmation = 'Confirmation';
  strError = 'Error';
  strChartTitle = 'plot of word frequency';
  strLeftTitle = 'frequency (log)';
  strBottomTitle = 'rank (log)';
  strProcessingMessage = 'Analyzing document - ';
  strReadyMessage = 'Ready';
  strNotAvailable = 'Not available for Esperanto';
  strOnlyAvailable = 'Available only in English';

function Percent(Val1, Val2: integer): string;
begin
  if (Val2 = 0) then
    Result := ' '
  else
    Result := FloatToStrF((Val1 / Val2) * 100.0, ffFixed, 5, 2);
end;

{ TMainForm }

procedure TMainForm.UpdateMenu(Sender: TObject);
begin
  FileSaveItem.Enabled := IsOpen;
  EditCopyItem.Enabled := IsFinished;
end;

procedure TMainForm.ScanAnalyze(fileName: string);
var
  TS: TWordFileStream;
  SC: TWordCounter;
  I, J, WordCount, Rank: longint;
  fileExt, fName, St: string;
  PI, H, {Freq,} SyllablesPerWord, CharsPerWord, WordsPerSentence,
  fleschReadability, fleschGradeLevel, gunningFog, colemanLiau, SMOG, ARI: double;
begin
  Screen.Cursor := crHourGlass;
  FormatSettings.DecimalSeparator := '.';

  fileExt := LowerCase(ExtractFileExt(fileName));
  case fileExt of
    '.txt':
      fName := fileName;
    '.htm', '.html', '.xhtml':
      fName := ConvertHtml(fileName);
    '.epub':
      fName := ConvertEpub(fileName);
    '.rtf':
      fName := ConvertRtf(fileName);
  end;

  SC := TWordCounter.Create;
  SC.SetLanguage(CurrentLang);
  try
    { Text processing }
    TS := TWordFileStream.Create(fName, fmOpenRead);
    try
      WordCount := 0;
      while TS.ReadWord(St) do
      begin
        Application.ProcessMessages; { Yield to other events }
        // Process word
        SC.AddWord(St);
        StatusLine.SimpleText :=
          strProcessingMessage + ExtractFileName(FileName) + '... ' + IntToStr(SC.Count);
        Inc(WordCount);
      end;
      TS.FinalizeStats;  // <<< guarantees last sentence
    finally
      TS.Free;
    end;

    { Avoid division by zero }
    if (SC.TotalCount = 0) or (SentenceCount = 0) then
    begin
      fleschReadability := 0;
      fleschGradeLevel := 0;
      colemanLiau := 0;
      gunningFog := 0;
      SMOG := 0;
      ARI := 0;
    end
    else
    begin
      { Averages }
      SyllablesPerWord := SC.SyllableCount / SC.TotalCount;
      CharsPerWord := AlphaNumChars / SC.TotalCount;
      WordsPerSentence := SC.TotalCount / SentenceCount;
    end;

    { Readability indices }

    if SC.Language = 'pt' then
    begin
      { Flesch adapted to Portuguese (Martins et al. 1996) }
      fleschReadability := 248.835 - 1.015 * (SC.TotalCount / SentenceCount) -
        84.6 * (SC.SyllableCount / SC.TotalCount);
    end
    else if SC.Language = 'es' then
    begin
      { Flesch adapted to Spanish (Fernández Huerta 1959) }
      fleschReadability := 206.84 - 0.60 * ((SC.SyllableCount / SC.TotalCount) * 100) -
        1.02 * (SC.TotalCount / SentenceCount);
    end
    else if SC.Language = 'it' then
    begin
      { Flesch adapted to Italian (Vacca & Franchina 1972) }
      fleschReadability := 217 - 1.3 * ((SC.SyllableCount / SC.TotalCount) * 100) -
        0.6 * (SC.TotalCount / SentenceCount);
    end
    else if SC.Language = 'de' then
    begin
      { Flesch adapted to German (Amstad 1978) }
      fleschReadability := 180 - (SC.TotalCount / SentenceCount) -
        58.5 * (SC.SyllableCount / SC.TotalCount);
    end
    else if SC.Language = 'fr' then
    begin
      { Flesch adapted to French (Kandel & Moles 1958) }
      fleschReadability := 207.0 - 1.015 * WordsPerSentence - 0.736 * SyllablesPerWord;
    end
    else
    begin
      { Flesch (original - English) }
      fleschReadability := 206.835 - 1.015 * (SC.TotalCount / SentenceCount) -
        84.6 * (SC.SyllableCount / SC.TotalCount);
    end;

    if fleschReadability < 0 then
      fleschReadability := 0
    else if fleschReadability > 100 then
      fleschReadability := 100;

    { Flesch-Kincaid Grade Level }
    fleschGradeLevel := 0.39 * (SC.TotalCount / SentenceCount) +
      11.8 * (SC.SyllableCount / SC.TotalCount) - 15.59;

    { Coleman-Liau }
    colemanLiau := 5.89 * (AlphaNumChars / SC.TotalCount) - 29.5 *
      (SentenceCount / SC.TotalCount) - 15.8;
    if (colemanLiau < 0.0) then
      colemanLiau := 0.0;

    { Gunning Fog }
    gunningFog := 0.4 * (SC.TotalCount / SentenceCount) + 100 *
      (SC.ComplexWordCount / SC.TotalCount);

    { SMOG }
    SMOG := 1.043 * Sqrt(30 * (SC.ComplexWordCount / SentenceCount)) + 3.1291;

    { ARI }
    ARI := 4.71 * (TotalChars / SC.TotalCount) + 0.5 *
      (SC.TotalCount / SentenceCount) - 21.43;
    if (ARI < 0.0) then
      ARI := 0.0;

    { Create html output string }
    Lines.Clear;
    Lines.Add('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">');
    Lines.Add('<html>');
    Lines.Add('<head>');
    Lines.Add('<title>Text Analysis of ' + ExtractFileName(fileName) + '</title>');
    Lines.Add('</head>');
    Lines.Add('<body>');
    Lines.Add('');
    Lines.Add('<h2>' + strtitle + ExtractFileName(fileName) + '</h2>');
    Lines.Add('<table border=1 cellspacing=1 cellpadding=1 width="80%">');
    Lines.Add('<tr><th>' + strRank + '</th>');
    Lines.Add('<th>' + strWord + '</th>');
    Lines.Add('<th>' + strFrequency + '</th>');
    Lines.Add('<th>%</th>');
    Lines.Add('<th>C</th>');
    Lines.Add('</tr>');

    { Frequency analysis }
    SC.SortByFrequency;
    Rank := 0;
    H := 0.0;
    for I := 0 to SC.Count - 1 do
    begin
      Rank := I + 1;
      PI := SC.FreqAt(I) / SC.TotalCount;
      H := H + PI * Log2(PI);
      {Freq := PI;}

      Lines.Add('<tr><td align="Center">' + IntToStr(Rank) + '</td>');
      Lines.Add('<td align="Center">' + LowerCase(SC.WordAt(I)) + '</td>');
      Lines.Add('<td align="Center">' + IntToStr(SC.FreqAt(I)) + '</td>');
      Lines.Add('<td align="Center">' + Percent(SC.FreqAt(I),
        SC.TotalCount) + '</td>');
      Lines.Add('<td align="Center">' + IntToStr(Rank * SC.FreqAt(I)) +
        '</td></tr>');

      // Limpa lista global antes de preencher
      if GlobalWordList = nil then
        GlobalWordList := TStringList.Create
      else
        GlobalWordList.Clear;

      // Cabeçalho CSV
      GlobalWordList.Add('Rank,Word,Frequency,Percent');

      // Preenche lista global
      Rank := 0;
      for J := 0 to SC.Count - 1 do
      begin
        Rank := J + 1;
        GlobalWordList.Add(
          IntToStr(Rank) + ',' + LowerCase(SC.WordAt(J)) + ',' +
          IntToStr(SC.FreqAt(J)) + ',' + Percent(SC.FreqAt(J), SC.TotalCount)
          );
      end;
    end;

    Lines.Add('</table>');
    Lines.Add('');
    Lines.Add('<h3>' + strStatText + '</h3>');
    Lines.Add('<table>');
    Lines.Add(Concat('<tr><td>', strTotalCount, '</td><td>',
      IntToStr(SC.TotalCount), '</td></tr>'));
    Lines.Add(Concat('<tr><td>', strCount, '</td><td>', IntToStr(SC.Count),
      '</td></tr>'));
    Lines.Add(Concat('<tr><td>', strPercent, '</td><td>',
      Percent(SC.Count, SC.TotalCount), '</td><tr>'));
    Lines.Add(Concat('<tr><td>', strTotalChars, '</td><td>',
      IntToStr(TotalChars), '</td></tr>'));
    Lines.Add(Concat('<tr><td>', strAlphaNumChars, '</td><td>',
      IntToStr(AlphaNumChars), '</td></tr>'));
    Lines.Add(Concat('<tr><td>', strNonSpaceChars, '</td><td>',
      IntToStr(NonSpaceChars), '</td></tr>'));
    Lines.Add(Concat('<tr><td>', strCharsPerWord, '</td><td>',
      FloatToStrF(CharsPerWord, ffFixed, 8, 5), '</td></tr>'));
    Lines.Add(Concat('<tr><td>', strLineCount, '</td><td>',
      IntToStr(LineCount), '</td></tr>'));
    Lines.Add(Concat('<tr><td>', strBlankLineCount, '</td><td>',
      IntToStr(BlankLineCount), '</td></tr>'));
    Lines.Add(Concat('<tr><td>', strParagraphs, '</td><td>',
      IntToStr(ParagraphCount), '</td></tr>'));
    Lines.Add(Concat('<tr><td>', strSentenceCount, '</td><td>',
      IntToStr(SentenceCount), '</td></tr>'));
    Lines.Add(Concat('<tr><td>', strWordsPerSentence, '</td><td>',
      FloatToStrF(WordsPerSentence, ffFixed, 8, 5), '</td></tr>'));
    Lines.Add(Concat('<tr><td>', strSyllableCount, '</td><td>',
      IntToStr(SC.SyllableCount), '</td></tr>'));
    Lines.Add(Concat('<tr><td>', strSyllablesPerWord, '</td><td>',
      FloatToStrF(SyllablesPerWord, ffFixed, 8, 5), '</td></tr>'));
    Lines.Add(Concat('<tr><td>', strComplexWordCount, '</td><td>',
      IntToStr(SC.ComplexWordCount), '</td></tr>'));
    Lines.Add('</table>');

    { Shannon-Wiener Index }
    H := H * (-1);
    Lines.Add('');
    Lines.Add('<h3>' + strStatInfo + '</h3>');
    Lines.Add('<table>');
    Lines.Add(Concat('<tr><td>Shannon-Wiener H'' = </td><td>',
      FloatToStrF(H, ffFixed, 7, 5), '</td></tr>'));
    Lines.Add('</table>');

    Lines.Add('');
    Lines.Add('<h3>' + strIndexes + '</h3>');
    Lines.Add('<table>');

    if SC.Language = 'eo' then
      Lines.Add(Concat('<tr><td>Flesch Readability Index = </td><td>',
        Concat('<i>', strNotAvailable, '</i>'), '</td></tr>'))
    else
      Lines.Add(Concat('<tr><td>Flesch Readability Index = </td><td>',
        FloatToStrF(fleschReadability, ffFixed, 7, 5), '</td></tr>'));

    if SC.Language = 'en' then
    begin
      Lines.Add(Concat('<tr><td>Flesch-Kincaid Grade Level = </td><td>',
        FloatToStrF(fleschGradeLevel, ffFixed, 7, 5), '</td></tr>'));
      Lines.Add(Concat('<tr><td>Gunning-Fog Index = </td><td>',
        FloatToStrF(gunningFog, ffFixed, 7, 5), '</td></tr>'));
      Lines.Add(Concat('<tr><td>SMOG Index = </td><td>',
        FloatToStrF(SMOG, ffFixed, 7, 5), '</td></tr>'));
      Lines.Add(Concat('<tr><td>Coleman-Liau Index = </td><td>',
        FloatToStrF(colemanLiau, ffFixed, 7, 5), '</td></tr>'));
      Lines.Add(Concat('<tr><td>Automated Readability Index = </td><td>',
        FloatToStrF(ARI, ffFixed, 7, 5), '</td></tr>'));
    end
    else
    begin
      Lines.Add(Concat('<tr><td>Flesch-Kincaid Grade Level = </td><td>',
        Concat('<i>', strOnlyAvailable, '</i>'), '</td></tr>'));
      Lines.Add(Concat('<tr><td>Gunning-Fog Index = </td><td>',
        Concat('<i>', strOnlyAvailable, '</i>'), '</td></tr>'));
      Lines.Add(Concat('<tr><td>SMOG Index = </td><td>',
        Concat('<i>', strOnlyAvailable, '</i>'), '</td></tr>'));
      Lines.Add(Concat('<tr><td>Coleman-Liau Index = </td><td>',
        Concat('<i>', strOnlyAvailable, '</i>'), '</td></tr>'));
      Lines.Add(Concat('<tr><td>Automated Readability Index = </td><td>',
        Concat('<i>', strOnlyAvailable, '</i>'), '</td></tr>'));
    end;

    Lines.Add('</table>');
    Lines.Add('</body>');
    Lines.Add('</html>');
    HtmlPanel.SetHtmlFromStr(Lines.Text);
    HtmlPanel.Visible := True;
    PlotIt(SC);
  finally
    SC.Free;
  end;
  StatusLine.SimpleText := strReadyMessage;
  Application.ProcessMessages;
  Screen.Cursor := crDefault;
  IsFinished := True;
  UpdateMenu(Self);
end;

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

procedure TMainForm.PlotIt(SC: TWordCounter);
var
  I, n, Rank: integer;
  Freq, H: double;
  XMin, XMax, YMin, YMax: double;
  Ex: TDoubleRect;
  Freqs: array of double;
begin
  LineSeries.Clear;

  n := SC.Count;
  if (n = 0) or (SC.TotalCount = 0) then
  begin
    Chart.Visible := False;
    Exit;
  end;

  // copiar frequências (proporções) para um array
  SetLength(Freqs, n);
  for I := 0 to n - 1 do
    Freqs[I] := SC.FreqAt(I) / SC.TotalCount; // double division

  // plotar na ordem do array (Rank = 1..n)
  XMin := 1E308;
  XMax := 0.0;
  YMin := 1E308;
  YMax := 0.0;
  H := 0.0;

  for I := 0 to n - 1 do
  begin
    Rank := I + 1;
    Freq := Freqs[I];
    if Freq > 0.0 then
      H := H + Freq * Log2(Freq);

    LineSeries.AddXY(double(Rank), Freq);

    XMin := Min(XMin, double(Rank));
    XMax := Max(XMax, double(Rank));
    YMin := Min(YMin, Freq);
    YMax := Max(YMax, Freq);
  end;

  // NÃO reativar ordenação automática da ListSource:
  // LineSeries.ListSource.Sorted := True; // <-- NÃO usar

  // ajustar eixos e rótulos
  Ex := LineSeries.Extent;
  CreateAxisLabels(XAxisLabels, Ex.a.x, Ex.b.x);
  CreateAxisLabels(YAxisLabels, Ex.a.y, Ex.b.y);

  Chart.BottomAxis.Marks.Source := XAxisLabels;
  Chart.BottomAxis.Marks.Style := smsLabel;
  Chart.LeftAxis.Marks.Source := YAxisLabels;
  Chart.LeftAxis.Marks.Style := smsLabel;

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
end;

procedure TMainForm.FileOpenItemClick(Sender: TObject);
begin
  OpenDialog.DefaultExt := '.txt';
  OpenDialog.Filter := strOpenText;
  if OpenDialog.Execute then
  begin
    MainForm.Caption := Application.Title + ' - ' + ExtractFileName(OpenDialog.Filename);
    StatusLine.SimpleText := strProcessingMessage +
      ExtractFileName(OpenDialog.Filename) + '...';
    ScanAnalyze(OpenDialog.Filename);
    StatusLine.SimpleText := strReadyMessage;
    HistoryFiles.AddToRecent(OpenDialog.FileName);
    // !! wp: replaces this:   HistoryFiles.UpdateList(OpenDialog.Filename);
    PageControl.ActivePage := ResultsTab;
    IsOpen := True;
    UpdateMenu(Self);
  end;
end;

procedure TMainForm.FileSaveItemClick(Sender: TObject);
var
  //Outfile: TextFile;
  OutPut: TStringList;
begin
  SaveDialog.Filename := '';
  if PageControl.ActivePage = ResultsTab then
  begin
    SaveDialog.DefaultExt := '.htm';
    SaveDialog.Filter := strSaveText;
    if SaveDialog.Execute then
    begin
      //AssignFile(Outfile, SaveDialog.Filename);
      //Rewrite(Outfile);
      {  // !! wp     -- WriteLn will crash the application in Windows! }
      case SaveDialog.FilterIndex of
        1: begin
          OutPut := TStringList.Create;
          try
            //Write(OutFile, StripHTML(Lines.Text));
            //Write(OutFile, Lines.Text);
            OutPut.Text := StripHTML(Lines.Text);
            OutPut.SaveToFile(SaveDialog.Filename);
          finally
            //CloseFile(OutFile);
            OutPut.Free;
          end;
        end;
        2: Lines.SaveToFile(SaveDialog.Filename);
        3: GlobalWordList.SaveToFile(SaveDialog.Filename);
      end;
      //CloseFile(Outfile);
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

  Historyfiles := TMRUMenuManager.Create(self);
  with HistoryFiles do
  begin
    IniFileName := sPath + 'Libro.ini';
    MenuItem := mnuRecentFiles;
    PopupMenu := RecentFilesPopup;
    OnRecentFile := @RecentFileClicked;
  end;
                                       { !! wp: is replaced by above code
  HistoryFiles := THistoryFiles.Create(self);
  with HistoryFiles do
  begin
    IniKey := 'History Files';
    ParentMenu := FileMenu;
    Separator := sepTop;
    Position := 6;
    FileMustExist := True;
    OnClickHistoryItem := @HistoryFilesClickHistoryItem;
    LocalPath := sPath;
    IniFile := sPath + 'Libro.ini';
    UpdateParentMenu;
  end;                                  }

  IniFile := TIniFile.Create(sPath + 'Libro.ini');
  sLang := IniFile.ReadString('Options', 'Language', 'en'); // First default is English
  SetDefaultLang(sLang, 'languages'); //, True);  // !! wp
  PageControl.ActivePage := ResultsTab;

  if sLang = 'en' then
  begin
    LanguageEnglishItem.Checked := True;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguageSpanishItem.Checked := False;
    LanguageFrenchItem.Checked := False;
    LanguageItalianItem.Checked := False;
    LanguageGermanItem.Checked := False;
    LanguageEsperantoItem.Checked := False;
  end;

  if sLang = 'pt-br' then
  begin
    LanguageEnglishItem.Checked := False;
    LanguagePortugueseBrazilianItem.Checked := True;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguageSpanishItem.Checked := False;
    LanguageFrenchItem.Checked := False;
    LanguageItalianItem.Checked := False;
    LanguageGermanItem.Checked := False;
    LanguageEsperantoItem.Checked := False;
  end;

  if sLang = 'pt' then
  begin
    LanguageEnglishItem.Checked := True;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguageSpanishItem.Checked := False;
    LanguageFrenchItem.Checked := False;
    LanguageItalianItem.Checked := False;
    LanguageGermanItem.Checked := False;
    LanguageEsperantoItem.Checked := False;
  end;

  if sLang = 'es' then
  begin
    LanguageEnglishItem.Checked := False;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguageSpanishItem.Checked := True;
    LanguageFrenchItem.Checked := False;
    LanguageItalianItem.Checked := False;
    LanguageGermanItem.Checked := False;
    LanguageEsperantoItem.Checked := False;
  end;

  if sLang = 'fr' then
  begin
    LanguageEnglishItem.Checked := False;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguageSpanishItem.Checked := False;
    LanguageFrenchItem.Checked := True;
    LanguageItalianItem.Checked := False;
    LanguageGermanItem.Checked := False;
    LanguageEsperantoItem.Checked := False;
  end;

  if sLang = 'it' then
  begin
    LanguageEnglishItem.Checked := False;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguageSpanishItem.Checked := False;
    LanguageFrenchItem.Checked := False;
    LanguageItalianItem.Checked := True;
    LanguageGermanItem.Checked := False;
    LanguageEsperantoItem.Checked := False;
  end;

  if sLang = 'de' then
  begin
    LanguageEnglishItem.Checked := False;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguageSpanishItem.Checked := False;
    LanguageFrenchItem.Checked := False;
    LanguageItalianItem.Checked := False;
    LanguageGermanItem.Checked := True;
    LanguageEsperantoItem.Checked := False;
  end;

  if sLang = 'eo' then
  begin
    LanguageEnglishItem.Checked := False;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguagePortugueseBrazilianItem.Checked := False;
    LanguageSpanishItem.Checked := False;
    LanguageFrenchItem.Checked := False;
    LanguageItalianItem.Checked := False;
    LanguageGermanItem.Checked := False;
    LanguageEsperantoItem.Checked := True;
  end;

  Lines := TStringList.Create;
  GlobalWordList := TStringList.Create;
  IsOpen := False;
  IsFinished := False;
  UpdateMenu(Self);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if FileExists('LIBRO.$$$') then
    DeleteFile('LIBRO.$$$');
  IniFile.Free;
  Lines.Free;
  GlobalWordList.Free;
end;

procedure TMainForm.HelpAboutItemClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TMainForm.RecentFileClicked(Sender: TObject; const FileName: string);
{ !! wp: renamed like above
procedure TMainForm.HistoryFilesClickHistoryItem(Sender: TObject;
  Item: TMenuItem; const Filename: string);
  }
begin
  if FileExists(Filename) then
  begin
    IsOpen := True;
    UpdateMenu(Self);
    MainForm.Caption := Application.Title + ' - ' + ExtractFileName(Filename);
    ScanAnalyze(Filename);
  end
  else
    Application.MessageBox(PChar(strNotFound), PChar(strError), mb_Ok or
      mb_IconExclamation);
end;

procedure TMainForm.LanguageEnglishItemClick(Sender: TObject);
begin
  SetDefaultLang('en', 'language'); // , True);   // !! wp
  CurrentLang := 'en';
  IniFile.WriteString('Options', 'Language', 'en');
  LanguageEnglishItem.Checked := True;
  LanguagePortugueseBrazilianItem.Checked := False;
  LanguagePortugueseItem.Checked := False;
  LanguageSpanishItem.Checked := False;
  LanguageFrenchItem.Checked := False;
  LanguageItalianItem.Checked := False;
  LanguageGermanItem.Checked := False;
  LanguageEsperantoItem.Checked := False;
end;

procedure TMainForm.LanguagePortugueseBrazilianItemClick(Sender: TObject);
begin
  SetDefaultLang('pt-br', 'language'); // , True);  // !! wp
  CurrentLang := 'pt-br';
  IniFile.WriteString('Options', 'Language', 'pt-br');
  LanguageEnglishItem.Checked := False;
  LanguagePortugueseBrazilianItem.Checked := True;
  LanguagePortugueseItem.Checked := False;
  LanguageSpanishItem.Checked := False;
  LanguageFrenchItem.Checked := False;
  LanguageItalianItem.Checked := False;
  LanguageGermanItem.Checked := False;
  LanguageEsperantoItem.Checked := False;
end;

procedure TMainForm.LanguageEsperantoItemClick(Sender: TObject);
begin
  SetDefaultLang('eo', 'language'); // , True);  // !! wp
  CurrentLang := 'eo';
  IniFile.WriteString('Options', 'Language', 'eo');
  LanguageEnglishItem.Checked := False;
  LanguagePortugueseBrazilianItem.Checked := False;
  LanguagePortugueseItem.Checked := False;
  LanguageSpanishItem.Checked := False;
  LanguageFrenchItem.Checked := False;
  LanguageItalianItem.Checked := False;
  LanguageGermanItem.Checked := False;
  LanguageEsperantoItem.Checked := True;
end;

procedure TMainForm.LanguageFrenchItemClick(Sender: TObject);
begin
  SetDefaultLang('fr', 'language'); // , True);  // !! wp
  CurrentLang := 'fr';
  IniFile.WriteString('Options', 'Language', 'fr');
  LanguageEnglishItem.Checked := False;
  LanguagePortugueseBrazilianItem.Checked := False;
  LanguagePortugueseItem.Checked := False;
  LanguageSpanishItem.Checked := True;
  LanguageFrenchItem.Checked := False;
  LanguageItalianItem.Checked := False;
  LanguageGermanItem.Checked := False;
  LanguageEsperantoItem.Checked := False;
end;

procedure TMainForm.LanguageGermanItemClick(Sender: TObject);
begin
  SetDefaultLang('de', 'language'); // , True);  // !! wp
  CurrentLang := 'de';
  IniFile.WriteString('Options', 'Language', 'de');
  LanguageEnglishItem.Checked := False;
  LanguagePortugueseBrazilianItem.Checked := False;
  LanguagePortugueseItem.Checked := False;
  LanguageSpanishItem.Checked := False;
  LanguageFrenchItem.Checked := False;
  LanguageItalianItem.Checked := False;
  LanguageGermanItem.Checked := True;
  LanguageEsperantoItem.Checked := False;
end;

procedure TMainForm.LanguageItalianItemClick(Sender: TObject);
begin
  SetDefaultLang('it', 'language'); // , True);  // !! wp
  CurrentLang := 'it';
  IniFile.WriteString('Options', 'Language', 'it');
  LanguageEnglishItem.Checked := False;
  LanguagePortugueseBrazilianItem.Checked := False;
  LanguagePortugueseItem.Checked := False;
  LanguageSpanishItem.Checked := False;
  LanguageFrenchItem.Checked := False;
  LanguageItalianItem.Checked := True;
  LanguageGermanItem.Checked := False;
  LanguageEsperantoItem.Checked := False;
end;

procedure TMainForm.LanguagePortugueseItemClick(Sender: TObject);
begin
  SetDefaultLang('pt', 'language'); // , True);  // !! wp
  CurrentLang := 'pt';
  IniFile.WriteString('Options', 'Language', 'pt');
  LanguageEnglishItem.Checked := False;
  LanguagePortugueseBrazilianItem.Checked := False;
  LanguagePortugueseItem.Checked := True;
  LanguageSpanishItem.Checked := False;
  LanguageFrenchItem.Checked := False;
  LanguageItalianItem.Checked := False;
  LanguageGermanItem.Checked := False;
  LanguageEsperantoItem.Checked := False;
end;

procedure TMainForm.LanguageSpanishItemClick(Sender: TObject);
begin
  SetDefaultLang('es', 'language'); // , True);  // !! wp
  CurrentLang := 'es';
  IniFile.WriteString('Options', 'Language', 'es');
  LanguageEnglishItem.Checked := False;
  LanguagePortugueseBrazilianItem.Checked := False;
  LanguagePortugueseItem.Checked := False;
  LanguageSpanishItem.Checked := True;
  LanguageFrenchItem.Checked := False;
  LanguageItalianItem.Checked := False;
  LanguageGermanItem.Checked := False;
  LanguageEsperantoItem.Checked := False;
end;

procedure TMainForm.FileExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.EditCopyItemClick(Sender: TObject);
begin
  if PageControl.ActivePage = ResultsTab then
    //    Clipboard.AsText := StripHTML(HTMLViewer.Text)  // DocumentSource)  // wp !! Text rather than DocumentSource?
    Clipboard.AsText := StripHTML(Lines.Text)
  else
    Chart.CopyToClipboardBitmap;
end;

end.
