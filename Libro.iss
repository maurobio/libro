; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{0AD13CC8-2D58-415E-9A91-7E2B73D2AFE6}
AppName=Libro
AppVersion=3.0.0
;AppVerName=Libro 2.0.0
AppPublisher=Rizoma Editorial
AppPublisherURL=http://librejo.sourceforge.net/
AppSupportURL=http://librejo.sourceforge.net/
AppUpdatesURL=http://librejo.sourceforge.net/
DefaultDirName={pf}\Libro
DisableDirPage=yes
DefaultGroupName=Libro
AllowNoIcons=yes
LicenseFile=userdocs:Google Drive\Projetos\Libro\gpl.txt
OutputBaseFilename=Libro-3.0.0-setup
Compression=lzma
SolidCompression=yes
WizardSmallImageFile=userdocs:Google Drive\Projetos\Libro\libro.bmp
AppCopyright=(c) 2013-2017 Mauro J. Cavalcanti
WizardImageStretch=False

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "C:\Documents and Settings\Mauro Cavalcanti\My Documents\Google Drive\Projetos\Libro\gpl.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "C:\Documents and Settings\Mauro Cavalcanti\My Documents\Google Drive\Projetos\Libro\Libro.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "C:\Documents and Settings\Mauro Cavalcanti\My Documents\Google Drive\Projetos\Libro\hound.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "C:\Documents and Settings\Mauro Cavalcanti\My Documents\Google Drive\Projetos\Libro\hound.epub"; DestDir: "{app}"; Flags: ignoreversion
Source: "C:\Documents and Settings\Mauro Cavalcanti\My Documents\Google Drive\Projetos\Libro\hound.html"; DestDir: "{app}"; Flags: ignoreversion
Source: "C:\Documents and Settings\Mauro Cavalcanti\My Documents\Google Drive\Projetos\Libro\languages\Libro.en.po"; DestDir: "{app}\languages"; Flags: ignoreversion
Source: "C:\Documents and Settings\Mauro Cavalcanti\My Documents\Google Drive\Projetos\Libro\languages\Libro.po"; DestDir: "{app}\languages"; Flags: ignoreversion
Source: "C:\Documents and Settings\Mauro Cavalcanti\My Documents\Google Drive\Projetos\Libro\languages\Libro.pt-br.po"; DestDir: "{app}\languages"; Flags: ignoreversion

[Icons]
Name: "{group}\Libro"; Filename: "{app}\Libro.exe"
Name: "{group}\{cm:ProgramOnTheWeb,Libro}"; Filename: "http://librejo.sourceforge.net/"
Name: "{group}\{cm:UninstallProgram,Libro}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\Libro"; Filename: "{app}\Libro.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\Libro.exe"; Description: "{cm:LaunchProgram,Libro}"; Flags: nowait postinstall skipifsilent

[Dirs]
Name: "{app}\languages"