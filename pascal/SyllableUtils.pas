unit SyllableUtils;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  SysUtils, Character;

function CountSyllables(const Word, Lang: string): Integer;

function CountSyllablesEn(const Word: string): Integer;
function CountSyllablesPt(const Word: string): Integer;
function CountSyllablesEs(const Word: string): Integer;
function CountSyllablesFr(const Word: string): Integer;
function CountSyllablesIt(const Word: string): Integer;
function CountSyllablesDe(const Word: string): Integer;

implementation

{ ---------- utilitários ---------- }

{ Converte string UTF-8 para WideString }
function ToWide(const S: string): WideString;
begin
  Result := UTF8Decode(S);
end;

{ Retorna somente os caracteres que são letras (preserva acentos) }
function OnlyLettersWide(const W: WideString): WideString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(W) do
    if IsLetter(W[i]) then
      Result := Result + W[i];
end;

{ ---------- funções IsVowel por idioma (sem usar sets) ---------- }

function IsVowelEn(c: WideChar): Boolean;
begin
  case c of
    'a','A','e','E','i','I','o','O','u','U','y','Y': Result := True
  else
    Result := False;
  end;
end;

function IsVowelPt(c: WideChar): Boolean;
begin
  case c of
    'a','A','á','Á','à','À','â','Â','ã','Ã',
    'e','E','é','É','ê','Ê',
    'i','I','í','Í',
    'o','O','ó','Ó','ô','Ô','õ','Õ',
    'u','U','ú','Ú','ü','Ü',
    'y','Y': Result := True
  else
    Result := False;
  end;
end;

function IsVowelEs(c: WideChar): Boolean;
begin
  case c of
    'a','A','á','Á',
    'e','E','é','É',
    'i','I','í','Í',
    'o','O','ó','Ó',
    'u','U','ú','Ú','ü','Ü',
    'y','Y': Result := True
  else
    Result := False;
  end;
end;

function IsVowelFr(c: WideChar): Boolean;
begin
  case c of
    'a','A','à','À','â','Â','ä','Ä',
    'e','E','é','É','è','È','ê','Ê','ë','Ë',
    'i','I','î','Î','ï','Ï',
    'o','O','ô','Ô','ö','Ö','œ','Œ','æ','Æ',
    'u','U','ù','Ù','û','Û','ü','Ü',
    'y','Y','ÿ','Ÿ': Result := True
  else
    Result := False;
  end;
end;

function IsVowelIt(c: WideChar): Boolean;
begin
  case c of
    'a','A','à','À',
    'e','E','è','È','é','É',
    'i','I','ì','Ì',
    'o','O','ò','Ò','ó','Ó',
    'u','U','ù','Ù',
    'y','Y': Result := True
  else
    Result := False;
  end;
end;

function IsVowelDe(c: WideChar): Boolean;
begin
  case c of
    'a','A','ä','Ä',
    'e','E',
    'i','I',
    'o','O','ö','Ö',
    'u','U','ü','Ü',
    'y','Y': Result := True
  else
    Result := False;
  end;
end;

{ ---------- contadores heurísticos por idioma ---------- }

function CountSyllablesEn(const Word: string): Integer;
var
  Ww: WideString;
  i, L: Integer;
begin
  Result := 0;
  if Word = '' then Exit;

  Ww := OnlyLettersWide(ToWide(Word));
  L := Length(Ww);
  if L = 0 then Exit;

  if IsVowelEn(Ww[1]) then Inc(Result);
  for i := 2 to L do
    if IsVowelEn(Ww[i]) and not IsVowelEn(Ww[i-1]) then
      Inc(Result);

  { 'e' mudo }
  if (L > 1) and ((Ww[L] = 'e') or (Ww[L] = 'E')) and (Result > 1) then
    Dec(Result);

  { final 'le' }
  if (L > 2) and ((Ww[L-1] = 'l') or (Ww[L-1] = 'L')) and ((Ww[L] = 'e') or (Ww[L] = 'E')) then
    if not IsVowelEn(Ww[L-2]) then
      Inc(Result);

  if Result = 0 then Result := 1;
end;

function CountSyllablesPt(const Word: string): Integer;
var
  Ww: WideString;
  i, L: Integer;
begin
  Result := 0;
  if Word = '' then Exit;

  Ww := OnlyLettersWide(ToWide(Word));
  L := Length(Ww);
  if L = 0 then Exit;

  if IsVowelPt(Ww[1]) then Inc(Result);
  for i := 2 to L do
    if IsVowelPt(Ww[i]) and not IsVowelPt(Ww[i-1]) then
      Inc(Result);

  if Result = 0 then Result := 1;
end;

function CountSyllablesEs(const Word: string): Integer;
var
  Ww: WideString;
  i, L: Integer;
begin
  Result := 0;
  if Word = '' then Exit;

  Ww := OnlyLettersWide(ToWide(Word));
  L := Length(Ww);
  if L = 0 then Exit;

  if IsVowelEs(Ww[1]) then Inc(Result);
  for i := 2 to L do
    if IsVowelEs(Ww[i]) and not IsVowelEs(Ww[i-1]) then
      Inc(Result);

  if Result = 0 then Result := 1;
end;

function CountSyllablesFr(const Word: string): Integer;
var
  Ww: WideString;
  i, L: Integer;
begin
  Result := 0;
  if Word = '' then Exit;

  Ww := OnlyLettersWide(ToWide(Word));
  L := Length(Ww);
  if L = 0 then Exit;

  if IsVowelFr(Ww[1]) then Inc(Result);
  for i := 2 to L do
    if IsVowelFr(Ww[i]) and not IsVowelFr(Ww[i-1]) then
      Inc(Result);

  { final "e" frequentemente mudo }
  if (L > 1) and ((Ww[L] = 'e') or (Ww[L] = 'E')) and (Result > 1) then
    Dec(Result);

  if Result = 0 then Result := 1;
end;

function CountSyllablesIt(const Word: string): Integer;
var
  Ww: WideString;
  i, L: Integer;
begin
  Result := 0;
  if Word = '' then Exit;

  Ww := OnlyLettersWide(ToWide(Word));
  L := Length(Ww);
  if L = 0 then Exit;

  if IsVowelIt(Ww[1]) then Inc(Result);
  for i := 2 to L do
    if IsVowelIt(Ww[i]) and not IsVowelIt(Ww[i-1]) then
      Inc(Result);

  if Result = 0 then Result := 1;
end;

function CountSyllablesDe(const Word: string): Integer;
var
  Ww: WideString;
  i, L: Integer;
begin
  Result := 0;
  if Word = '' then Exit;

  Ww := OnlyLettersWide(ToWide(Word));
  L := Length(Ww);
  if L = 0 then Exit;

  if IsVowelDe(Ww[1]) then Inc(Result);
  for i := 2 to L do
    if IsVowelDe(Ww[i]) and not IsVowelDe(Ww[i-1]) then
      Inc(Result);

  if Result = 0 then Result := 1;
end;

{ ---------- dispatcher ---------- }

function CountSyllables(const Word, Lang: string): Integer;
var
  Code: string;
begin
  Code := LowerCase(Trim(Lang));

  case Code of
    'pt', 'pt-br', 'pt-pt':
      Result := CountSyllablesPt(Word);
    'es', 'es-es', 'es-mx':
      Result := CountSyllablesEs(Word);
    'fr', 'fr-fr', 'fr-ca':
      Result := CountSyllablesFr(Word);
    'it', 'it-it':
      Result := CountSyllablesIt(Word);
    'de', 'de-de':
      Result := CountSyllablesDe(Word);
    'en', 'en-us', 'en-gb':
      Result := CountSyllablesEn(Word);
  else
    Result := CountSyllablesEn(Word); // fallback para inglês
  end;
end;

end.
