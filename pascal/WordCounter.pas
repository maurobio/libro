unit WordCounter;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Character, SyllableUtils;

type
  TWordCounter = class
  private
    FList: TStringList;
    FSyllableCount: integer;
    FComplexWordCount: integer;
    FTotalCount: integer;
    FLang: string;
    //function IsValidWord(const W: string): boolean;
    function IsValidWord(const S: string): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddWord(const W: string);
    function Count: integer;            // número de palavras distintas
    function TotalCount: integer;       // número total de palavras (com repetições)
    function WordAt(Index: integer): string;
    function FreqAt(Index: integer): integer;
    procedure SortByFrequency;          // ordena por frequência (decrescente)
    procedure SetLanguage(const Lang: string);
    property Language: string read FLang write SetLanguage;
    property SyllableCount: integer read FSyllableCount;
    property ComplexWordCount: integer read FComplexWordCount;
  end;

implementation

{ Comparador para ordenação por frequência (decrescente), desempata por texto (case-insensitive) }
function CompareByFrequency(List: TStringList; Index1, Index2: integer): integer;
var
  F1, F2: integer;
begin
  F1 := PtrInt(List.Objects[Index1]);
  F2 := PtrInt(List.Objects[Index2]);
  if F1 > F2 then
    Result := -1
  else if F1 < F2 then
    Result := 1
  else
    Result := AnsiCompareText(List[Index1], List[Index2]); // desempate alfabético
end;

{ TWordCounter }

constructor TWordCounter.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  FList.Sorted := False;      // NÃO manter ordenação automática por chave
  FList.Duplicates := dupIgnore;
  FSyllableCount := 0;
  FComplexWordCount := 0;
  FTotalCount := 0;
  FLang := 'en'; // padrão inglês
end;

destructor TWordCounter.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TWordCounter.SetLanguage(const Lang: string);
var
  Code: string;
begin
  Code := LowerCase(Trim(Lang));

  // idiomas suportados (use siglas completas, como 'pt-br', 'pt-pt', etc.)
  if (Code = 'en') or (Code = 'pt') or (Code = 'pt-br') or (Code = 'pt-pt') or
    (Code = 'fr') or (Code = 'it') or (Code = 'es') or (Code = 'de') then
    FLang := Code
  else
    FLang := 'en'; // fallback
end;

function TWordCounter.IsValidWord(const S: string): boolean;
var
  I: integer;
begin
  Result := (S <> '');
  if not Result then Exit;

  // rejeita se for puramente numérico
  Result := False;
  if TryStrToInt(S, I) then Exit;

  // rejeita se contiver números misturados a letras
  for I := 1 to Length(S) do
    //if not (S[I] in ['A'..'Z', 'a'..'z', 'À'..'Ö', 'Ø'..'ö', 'ø'..'ÿ']) then
    if not (S[I] in ['A'..'Z', 'a'..'z']) then
      Exit;

  Result := True;
end;

(*
function TWordCounter.IsValidWord(const W: string): boolean;
var
  Ch: char;
  HasLetter: boolean;
begin
  Result := False;
  if W = '' then Exit;

  HasLetter := False;
  for Ch in W do
  begin
    if IsLetter(Ch) then
      HasLetter := True
    else if IsDigit(Ch) then
    begin
      // rejeita qualquer token que contenha dígito
      Exit;
    end
    else
    begin
      // permite apóstrofo simples e hífen interno/apóstrofo Unicode (’)
      if not (Ch in ['''', '-']) then
      begin
        // permite também apóstrofo tipográfico (’ U+2019)
        if Ch <> #$2019 then
          Exit;
      end;
    end;
  end;

  Result := HasLetter; // aceita somente se houver pelo menos uma letra
end;
*)

procedure TWordCounter.AddWord(const W: string);
var
  Idx: integer;
  Sylls: integer;
begin
  if W = '' then Exit;
  if not IsValidWord(W) then Exit;

  { conta sílabas no idioma atual }
  Sylls := CountSyllables(W, FLang);

  // atualiza tabela de frequências
  Idx := FList.IndexOf(W);
  if Idx = -1 then
    FList.AddObject(W, TObject(PtrInt(1)))
  else
    FList.Objects[Idx] := TObject(PtrInt(PtrInt(FList.Objects[Idx]) + 1));

  // atualiza contadores globais
  Inc(FTotalCount);
  Inc(FSyllableCount, Sylls);
  if Sylls >= 3 then
    Inc(FComplexWordCount);
end;

function TWordCounter.Count: integer;
begin
  Result := FList.Count;
end;

function TWordCounter.TotalCount: integer;
begin
  Result := FTotalCount;
end;

function TWordCounter.WordAt(Index: integer): string;
begin
  Result := FList[Index];
end;

function TWordCounter.FreqAt(Index: integer): integer;
begin
  Result := PtrInt(FList.Objects[Index]);
end;

procedure TWordCounter.SortByFrequency;
begin
  // assegura que a lista não reordene automaticamente por chave
  FList.Sorted := False;
  FList.CustomSort(@CompareByFrequency);
  FList.Sorted := False;
end;

end.
