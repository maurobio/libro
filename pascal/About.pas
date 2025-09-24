unit About;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF Windows} Win32Proc, {$ENDIF} FileInfo,
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, LCLTranslator, LCLVersion, LCLIntf;

type

  { TAboutBox }
  TAboutBox = class(TForm)
    Compiler: TLabel;
    ProgramIcon: TImage;
    Logo: TImage;
    Website: TLabel;
    VersionNumber: TLabel;
    OperatingSystem: TLabel;
    Panel1: TPanel;
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
  strComments = 'This program scans a whole text file (in plain text, HTML, or EPUB formats '
    + 'and ranks all used words according to frequency, performing a quantitative analysis of the '
    + 'text using Shannon-Weaver information statistic and Zipf power law function. It counts '
    + 'words, sentences, chars, spaces, and syllables. Also computes readability indexes '
    + '(Gunning-Fog, Coleman-Liau, Automated Readability Index (ARI), SMOG grade, ' +
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
  else
    OSVersion := 'Windows ';
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
  Compiler.Caption := Format(strCompiler, [{$I %FPCVERSION%},
    lcl_major, lcl_minor, {$I %FPCTARGETCPU%}, {$I %FPCTARGETOS%}]);
  //OperatingSystem.Caption := 'OS: ' + GetOS + ' (' + SystemStem + MajVer + '.' + MinVer + ')';
  OperatingSystem.Caption := 'OS: ' + OSVersion;
  Website.Caption := strWebsite;
end;

procedure TAboutBox.LogoClick(Sender: TObject);
begin
  OpenURL('http://www.comcienciaeditorial.wordpress.com');
end;

procedure TAboutBox.WebsiteClick(Sender: TObject);
begin
  OpenURL('http://librejo.sourceforge.net');
end;

{ TAboutBox }

end.
