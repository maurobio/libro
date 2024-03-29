{ Version 050601. Copyright � Alexey A.Chernobaev, 1996-2005 }

{$IFNDEF V_SORT_DESC}
procedure TNumberVector.Sort;
{$ELSE}
procedure TNumberVector.SortDesc;
{$ENDIF}
var
  I, N, Size: Integer;
  T: Byte;
  NewItems: PRegular;
  C: array [Byte] of Integer;
begin
  Size:=ItemsSize;
  if (FCount > 40) { for very short vectors quick sort is better }
    and
    (UInt32(Size) <= PhysicalMemorySize div 16)
  then begin
    GetMem(NewItems, Size);
    try
      SetNull(C, SizeOf(C));
      N:=FCount - 1;
      for I:=0 to N do
        Inc(C[Lo(PArrayType(FItems)^[I])]);
      for I:=Low(C) + 1 to High(C) do
        Inc(C[I], C[I - 1]);
      for I:=N downto 0 do begin
        T:=Lo(PArrayType(FItems)^[I]);
        Dec(C[T]);
        PArrayType(NewItems)^[C[T]]:=PArrayType(FItems)^[I];
      end;
      SetNull(C, SizeOf(C));
      for I:=0 to N do
        Inc(C[Hi(PArrayType(NewItems)^[I])]);
      for I:=Low(C) + 1 to High(C) do
        Inc(C[I], C[I - 1]);
      for I:=N downto 0 do begin
        T:=Hi(PArrayType(NewItems)^[I]);
        Dec(C[T]);
        PArrayType(FItems)^[C[T]]:=PArrayType(NewItems)^[I];
      end;
      {$IFDEF V_SORT_DESC}
      Reverse;
      {$ENDIF}
    finally
      FreeMem(NewItems, Size);
    end;
  end
  else
    inherited {$IFNDEF V_SORT_DESC}Sort{$ELSE}SortDesc{$ENDIF};
end;
