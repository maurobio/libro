unit WordReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Character;

var
  TotalChars: integer = 0;       // total de caracteres
  AlphaNumChars: integer = 0;    // letras e dígitos
  NonSpaceChars: integer = 0;    // não espaços
  LineCount: integer = 0;        // número de linhas
  ParagraphCount: integer = 0;   // número de parágrafos
  SentenceCount: integer = 0;    // número de sentenças
  BlankLineCount: integer = 0;   // número de linhas em branco

type
  TWordFileStream = class(TFileStream)
  private
    FBuffer: array[0..4095] of char; // 4KB buffer
    //FBuffer: array[0..4095] of byte;   // <<< aqui!
    FBufferPos: integer;
    FBufferSize: integer;
    FEOF: boolean;
    FCurrentChar: char;
    FHasCurrentChar: boolean;
    FLastWasLineBreak: boolean;
    FLastWasSentenceEnd: boolean;  // flag para sentenças
    function GetNextChar: boolean;
    function IsWordSeparator(C: char): boolean;
  public
    constructor Create(const AFileName: string; Mode: word);
    function ReadWord(out AWord: string): boolean;
    procedure FinalizeStats; // chamada no fim para consolidar sentenças
  end;

implementation

constructor TWordFileStream.Create(const AFileName: string; Mode: word);
begin
  inherited Create(AFileName, Mode);
  FBufferPos := 0;
  FBufferSize := 0;
  FEOF := False;
  FHasCurrentChar := False;
  FLastWasLineBreak := False;
  FLastWasSentenceEnd := False;

  // assumimos pelo menos 1 linha e 1 parágrafo se o arquivo não estiver vazio
  TotalChars := 0;
  AlphaNumChars := 0;
  NonSpaceChars := 0;
  LineCount := 0;
  ParagraphCount := 0;
  SentenceCount := 0;
  BlankLineCount := 0;
end;

function TWordFileStream.GetNextChar: boolean;
begin
  if FHasCurrentChar then
  begin
    Result := True;
    Exit;
  end;

  if FBufferPos >= FBufferSize then
  begin
    // Refill buffer
    FBufferSize := Read(FBuffer, SizeOf(FBuffer));
    FBufferPos := 0;

    if FBufferSize = 0 then
    begin
      FEOF := True;
      Result := False;
      Exit;
    end;

    // primeira leitura de um arquivo não vazio → 1 linha e 1 parágrafo
    if (TotalChars = 0) then
    begin
      LineCount := 1;
      ParagraphCount := 1;
    end;
  end;

  FCurrentChar := FBuffer[FBufferPos];
  //FCurrentChar := char(FBuffer[FBufferPos]); // <<< aqui!
  Inc(FBufferPos);
  FHasCurrentChar := True;

  // === Atualiza estatísticas globais ===
  Inc(TotalChars);
  if IsLetterOrDigit(FCurrentChar) then
    Inc(AlphaNumChars);
  if not IsWhiteSpace(FCurrentChar) then
    Inc(NonSpaceChars);

  // --- Contagem de linhas e parágrafos ---
  if (FCurrentChar = #10) then  // LF
  begin
    Inc(LineCount);

    if FLastWasLineBreak then
    begin
      Inc(ParagraphCount); // linha em branco → novo parágrafo
      Inc(BlankLineCount);  // <<< conta linha em branco
      FLastWasLineBreak := False;
    end
    else
      FLastWasLineBreak := True;
  end
  else if not (FCurrentChar in [#13]) then
  begin
    // reset do estado de linha em branco
    FLastWasLineBreak := False;
  end;

  // --- Contagem de sentenças (elegante) ---
  if (FCurrentChar in ['.', '!', '?']) then
  begin
    // marca possível fim de sentença
    FLastWasSentenceEnd := True;
  end
  else if not IsWhiteSpace(FCurrentChar) then
  begin
    // se encontramos palavra depois da pontuação → confirma sentença
    if FLastWasSentenceEnd then
    begin
      Inc(SentenceCount);
      FLastWasSentenceEnd := False;
    end;
  end;

  Result := True;
end;

procedure TWordFileStream.FinalizeStats;
begin
  // Se o arquivo terminou logo após um '.' '!' ou '?'
  if FLastWasSentenceEnd then
  begin
    Inc(SentenceCount);
    FLastWasSentenceEnd := False;
  end;
end;

function TWordFileStream.IsWordSeparator(C: char): boolean;
begin
  Result := C in [' ', #9, #10, #13, ',', '.', ';', ':', '!', '?',
    '"', '''', '(', ')', '[', ']', '{', '}', '<', '>', '/', '\',
    '|', '-', '_', '=', '+', '*', '&', '%', '$', '#', '@'];
end;

function TWordFileStream.ReadWord(out AWord: string): boolean;
var
  C: char;
  WordStarted: boolean;
begin
  AWord := '';
  Result := False;
  WordStarted := False;

  // Skip leading separators
  while GetNextChar do
  begin
    C := FCurrentChar;
    FHasCurrentChar := False;

    if not IsWordSeparator(C) then
    begin
      // Start of a word
      AWord := C;
      WordStarted := True;
      Break;
    end;
  end;

  if not WordStarted and FEOF then
    Exit; // No more words

  // Read the rest of the word
  while GetNextChar do
  begin
    C := FCurrentChar;

    if IsWordSeparator(C) then
    begin
      FHasCurrentChar := True; // Put the separator back for next word
      Break;
    end
    else
    begin
      AWord := AWord + C;
      FHasCurrentChar := False;
    end;
  end;

  Result := AWord <> '';
end;

end.
