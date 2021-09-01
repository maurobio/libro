{============================================================================}
{                                Libro                                       }
{     A cross-platform program for statistical analysis of texts             }
{   using Shannon-Weaver information theory and Zipf power law function      }
{   and computation of readability indices (Flesch, Flesch-Kincaid, SMOG,    }
{      Gunning-Fog, Coleman-Liau, and Automated Readability Index)           }
{                                                                            }
{              Copyright 2013-2017 Mauro J. Cavalcanti                       }
{                        maurobio@gmail.com                                  }
{                                                                            }
{   This program is free software: you can redistribute it and/or modify     }
{   it under the terms of the GNU General Public License as published by     }
{   the Free Software Foundation, either version 3 of the License, or        }
{   (at your option) any later version.                                      }
{                                                                            }
{   This program is distributed in the hope that it will be useful,          }
{   but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            }
{   GNU General Public License for more details.                             }
{                                                                            }
{   You should have received a copy of the GNU General Public License        }
{   along with this program. If not, see <http://www.gnu.org/licenses/>.     }
{                                                                            }
{   Requirements:                                                            }
{     Lazarus 1.6+ (www.lazarus.freepascal.org)                              }
{     Free Pascal 3.0+ (www.freepascal.org)                                  }
{     HTMLViewer 11.8+ (wiki.lazarus.freepascal.org/THtmlPort)               }
{     HistoryFiles 1.3+ (wiki.freepascal.org/HistoryFiles)                   }
{     Vector Library 050702+ (torry.net/vcl/science/vector/achvectors.zip)   }
{                                                                            }
{============================================================================}
unit About;

{$mode objfpc}{$H+}

interface

uses
    {$IFDEF Windows} Win32Proc, {$ENDIF} FileInfo,
    SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
    Buttons, ExtCtrls, LCLTranslator, LCLVersion, LCLIntf;

type

    { TAboutBox }

    TAboutBox = class (TForm)
      Compiler: TLabel;
      Logo: TImage;
      Website: TLabel;
      VersionNumber: TLabel;
      OperatingSystem: TLabel;
              Panel1: TPanel;
              ProgramIcon: TImage;
              ProductName: TLabel;
              Version: TLabel;
              Copyright: TLabel;
              Comments: TLabel;
              OKButton: TButton;
              procedure FormShow(Sender: TObject);
              procedure LogoClick(Sender: TObject);
              procedure WebsiteClick(Sender: TObject);
    private
         { Private declarations }
    public
        { Public declarations }
    end;

    Str255 = string[255];

var
   AboutBox: TAboutBox;

implementation

{$R *.lfm}

resourcestring
   strComments = 'This program scans a whole text file (in plain text, HTML, or EPUB formats ' +
     'and ranks all used words according to frequency, performing a quantitative analysis of the ' +
     'text using Shannon-Weaver information statistic and Zipf power law function. It counts ' +
     'words, sentences, chars, spaces, and syllables. Also computes readability indexes ' +
     '(Gunning-Fog, Coleman-Liau, Automated Readability Index (ARI), SMOG grade, ' +
     'Flesch-Kincaid grade level and Flesch Reading Ease).';
   strCompiler = 'Compiled with Free Pascal v.%s and Lazarus v.%d.%d for the %s -' +
    ' %s platform';
   strWebsite = 'Libro website';

//function GetOS: string;
//begin
//  Result := {$I %FPCTARGETOS%};
//end;

function OSVersion: Str255;
begin
  {$IFDEF LCLcarbon}
  OSVersion := 'Mac OS X 10.';
  {$ELSE}
  {$IFDEF Linux}
  OSVersion := 'Linux Kernel ';
  {$ELSE}
  {$IFDEF UNIX}
  OSVersion := 'Unix ';
  {$ELSE}
  {$IFDEF WINDOWS}
  if WindowsVersion = wv95 then OSVersion := 'Windows 95 '
   else if WindowsVersion = wvNT4 then OSVersion := 'Windows NT v.4 '
   else if WindowsVersion = wv98 then OSVersion := 'Windows 98 '
   else if WindowsVersion = wvMe then OSVersion := 'Windows ME '
   else if WindowsVersion = wv2000 then OSVersion := 'Windows 2000 '
   else if WindowsVersion = wvXP then OSVersion := 'Windows XP '
   else if WindowsVersion = wvServer2003 then OSVersion := 'Windows Server 2003 '
   else if WindowsVersion = wvVista then OSVersion := 'Windows Vista '
   else if WindowsVersion = wv7 then OSVersion := 'Windows 7 '
   else if WindowsVersion = wv8 then OSVersion := 'Windows 8 '
   else if WindowsVersion = wv10 then OSVersion := 'Windows 10 '
   else OSVersion:= 'Windows ';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

{ TAboutBox }

procedure TAboutBox.FormShow(Sender: TObject);
var
  //SystemStem, MajVer, MinVer: Str255;
  Quad: TVersionQuad;
begin
  if GetProgramVersion(Quad) then
    VersionNumber.Caption := VersionQuadToStr(Quad);
  Comments.Caption := strComments;
  //SystemStem := OSVersion;
  //{$IFDEF WINDOWS}
  //MajVer := IntToStr(Win32MajorVersion);
  //MinVer := IntToStr(Win32MinorVersion);
  //{$ELSE}
  //MajVer := IntToStr(Lo(DosVersion) - 4);
  //MinVer := IntToStr(Hi(DosVersion));
  //{$ENDIF}
  Compiler.Caption := Format(strCompiler,
    [{$I %FPCVERSION%}, lcl_major, lcl_minor, {$I %FPCTARGETCPU%}, {$I %FPCTARGETOS%}]);
  //OperatingSystem.Caption := 'OS: ' + GetOS + ' (' + SystemStem + MajVer + '.' + MinVer + ')';
  OperatingSystem.Caption := 'OS: ' + OSVersion;
  Website.Caption := strWebsite;
end;

procedure TAboutBox.LogoClick(Sender: TObject);
begin
  OpenURL('http://www.rizomaeditorial.com');
end;

procedure TAboutBox.WebsiteClick(Sender: TObject);
begin
  OpenURL('http://librejo.sourceforge.net');
end;

{ TAboutBox }

end.
