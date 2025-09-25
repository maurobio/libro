unit Convert;

interface

uses
  Classes, SysUtils, {StrUtils,} FileUtil, Zipper, DOM, XMLRead;

function ReadFile(const FileName: string): string;
function StripHTML(const S: string): string;
function StripRTF(const rtf: string): string;
function ConvertHtml(const fileName: string): string;
function ConvertEpub(const fileName: string): string;
function ConvertRtf(const fileName: string): string;
function ConvertOdt(const fileName: string): string;

implementation

{ Helper routines }
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

(*
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
*)

function StripHTML(const S: string): string;
var
  I: integer;
  InTag: boolean;
  C: char;
begin
  Result := '';
  InTag := False;
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '<': InTag := True;
      '>': InTag := False;
      else
        if not InTag then
          Result := Result + C;
    end;
  end;
end;

function StripRTF(const rtf: string): string;
var
  I, L: integer;
  c: char;
  inCommand, inGroup: boolean;
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    inCommand := False;
    inGroup := False;
    I := 1;
    L := Length(rtf);

    while I <= L do
    begin
      c := rtf[I];

      case c of
        '{':
          inGroup := True;  // início de grupo de formatação
        '}':
          inGroup := False; // fim de grupo
        '\':
        begin
          inCommand := True;
          // pular palavra de comando
          Inc(I);
          while (I <= L) and (rtf[I] in ['a'..'z', 'A'..'Z']) do
            Inc(I);
          // pode vir número opcional
          while (I <= L) and (rtf[I] in ['0'..'9', '-']) do
            Inc(I);
          // espaço opcional após comando
          if (I <= L) and (rtf[I] = ' ') then
            Inc(I);
          inCommand := False;
          Continue; // já avançamos I manualmente
        end;
        else
          if (not inGroup) and (not inCommand) then
            sb.Append(c);
      end;

      Inc(I);
    end;

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

{ General-purpose routines }
function ConvertHtml(const fileName: string): string;
var
  txt: string;
  OutFile: TextFile;
  fName: string;
begin
  fName := 'LIBRO.$$$';
  txt := StripHTML(ReadFile(fileName));  // já tem StripHTML no seu projeto
  AssignFile(OutFile, fName);
  Rewrite(OutFile);
  Write(OutFile, txt);
  CloseFile(OutFile);
  Result := fName;
end;

function ConvertEpub(const fileName: string): string;
var
  UnZipper: TUnZipper;
  FileList: TStringList;
  I: integer;
  txt, fName: string;
  OutFile: TextFile;
begin
  fName := 'LIBRO.$$$';
  txt := '';

  UnZipper := TUnZipper.Create;
  FileList := TStringList.Create;
  try
    // extrair para pasta temporária
    UnZipper.FileName := fileName;
    UnZipper.OutputPath := '.\tmp';
    UnZipper.Examine;
    UnZipper.UnZipAllFiles;

    // localizar todos os HTML/XHTML dentro do EPUB
    FileList := FindAllFiles('.\tmp', '*.htm;*.html;*.xhtml', True);
    for I := 0 to FileList.Count - 1 do
      txt := txt + StripHTML(ReadFile(FileList[I])) + sLineBreak;

    // salvar em arquivo temporário unificado
    AssignFile(OutFile, fName);
    Rewrite(OutFile);
    Write(OutFile, txt);
    CloseFile(OutFile);

  finally
    FileList.Free;
    UnZipper.Free;
    // limpar diretório temporário
    DeleteDirectory('.\tmp', True);
    RemoveDir('.\tmp');
  end;

  Result := fName;
end;

function ConvertRtf(const fileName: string): string;
var
  rtf, plain, fName: string;
  OutFile: TextFile;
begin
  fName := 'LIBRO.$$$';
  rtf := ReadFile(fileName);   // você já tem ReadFile no projeto
  plain := StripRTF(rtf);

  AssignFile(OutFile, fName);
  Rewrite(OutFile);
  Write(OutFile, plain);
  CloseFile(OutFile);

  Result := fName;
end;

function ConvertOdt(const FileName: string): string;
var
  Unzipper: TUnZipper;
  TempDir, ContentFile: string;
  Doc: TXMLDocument;
  Nodes: TDOMNodeList;
  I: Integer;
begin
  Result := '';
  // diretório temporário exclusivo
  TempDir := GetTempDir(False) + 'odt_extract_' + IntToStr(Random(MaxInt)) + PathDelim;
  ForceDirectories(TempDir);

  Unzipper := TUnZipper.Create;
  try
    Unzipper.FileName := FileName;
    Unzipper.OutputPath := TempDir;

    // Extrai todo o conteúdo do ODT no diretório temporário
    Unzipper.UnZipAllFiles;

    // Procura content.xml no diretório temporário (normalmente está na raiz do zip)
    ContentFile := TempDir + 'content.xml';
    if not FileExists(ContentFile) then
    begin
      // caso raro: talvez o arquivo tenha sido extraído para subpastas - tentar localizar
      if FindAllFiles(TempDir, 'content.xml', True).Count > 0 then
        ContentFile := FindAllFiles(TempDir, 'content.xml', True).Strings[0]
      else
        raise Exception.Create('Arquivo content.xml não encontrado dentro do .odt');
    end;

    // lê o XML
    ReadXMLFile(Doc, ContentFile);
    try
      // Extrai parágrafos e cabeçalhos
      Nodes := Doc.GetElementsByTagName('text:p');
      for I := 0 to Nodes.Count - 1 do
        if Assigned(Nodes[I].FirstChild) then
          Result := Result + Trim(Nodes[I].TextContent) + LineEnding;

      Nodes := Doc.GetElementsByTagName('text:h');
      for I := 0 to Nodes.Count - 1 do
        if Assigned(Nodes[I].FirstChild) then
          Result := Result + Trim(Nodes[I].TextContent) + LineEnding;
    finally
      Doc.Free;
    end;

  finally
    Unzipper.Free;
    // remove diretório temporário (recursivamente)
    if DirectoryExists(TempDir) then
      DeleteDirectory(TempDir, True); // precisa FileUtil
  end;
end;

end.
