{v.050627}
{
  ����������� ������ �����������: ������ �������������� Delphi � Free Pascal.
  Determining compiler version: Delphi and Free Pascal are now supported.
}
{$IFDEF FPC}
  {$DEFINE V_FREEPASCAL}
  {$MODE DELPHI}
  {$GOTO ON}
{$ELSE}
  {$DEFINE V_DELPHI}
{$ENDIF}
{
  VDEBUG: �������� ��� ��������� run-time ��������.
  VDEBUG: enable all possible run-time checks.
}
{.$DEFINE VDEBUG}
{
  CHECK_VECTORS: �������� run-time �������� � ���������� Vectors.
  CHECK_VECTORS: enable run-time checks in Vectors library.
}
{.$DEFINE CHECK_VECTORS}
{
  ���� � ������ ���� �� ���� ������� ����������, ����������� ��������������
  �������� ������������� ���� Float (������ ExtType), ������ ���� ��������.
  One and only one from the next three conditions controlling presentation of
  the generic real type Float (unit ExtType) must be enabled.
}
{.$DEFINE FLOAT_EQ_FLOAT32}
{$DEFINE FLOAT_EQ_FLOAT64}
{.$DEFINE FLOAT_EQ_FLOAT80}
{
  Run-time �������� ����� ���������� �� �����������.
  Run-time checks can be enabled independently.
}
{$IFDEF LINUX}
  {$IFNDEF FPC}
    {$DEFINE V_KYLIX}
  {$ENDIF}
{$ENDIF}
{$IFDEF VDEBUG}
  {$IFDEF V_FREEPASCAL}
    { ��� ������������� Free Pascal ���������� ��������� range-check }
    { range-check must be turned off when using Free Pascal }
    {$R-}
  {$ELSE}
    {$R+}
  {$ENDIF}
  {$Q+}
  {$S+}
  {$IFNDEF V_FREEPASCAL}
    {$IFNDEF VER80}
      {$O-}
    {$ENDIF}
  {$ENDIF}
  {$DEFINE CHECK_VECTORS}
  {$DEFINE CHECK_MATRIXES}
  {$DEFINE CHECK_ATTRS}
  {$DEFINE CHECK_GRAPHS}
  {$DEFINE CHECK_MATH}
{$ENDIF}
{
  �������� ����� �������������, ����������� ������������� ���������� �������
  (��������, �������, ������� ������� � ��.). ��������� ������������� �����
  ������� ������������, ������������� ��������� ������ ������� ������, �����
  ��������� ��������� ����� ����������� ������������ ������.
  Enable compatibility mode allowing use of deprecate facilities (procedures,
  functions, class methods, etc.). It is recommended to disable this condition
  when it's possible to update program code written earlier.
}
{.$DEFINE V_ALLOW_DEPRECATE}
{
  ����������� ������ �����������.
  Compiler version detection.
}
{$IFDEF V_FREEPASCAL}       { Free Pascal }
  {$DEFINE V_32}            { 32-bit environment }
  {$IFDEF WIN32}
    {$DEFINE V_WIN}
    {$DEFINE V_WIN32}
  {$ENDIF}
{$ENDIF}
{$IFDEF V_DELPHI}           { Delphi }
  {$IFNDEF LINUX}
    {$DEFINE V_WIN}
  {$ENDIF}
  {$DEFINE INT64_EQ_COMP}   { will be undefined for Delphi >= 4.0 (see below) }
  {$IFNDEF VER80}           { Delphi 1.0 }
    {$DEFINE V_32}          { Delphi >= 2.0: 32-bit environment }
    {$IFNDEF LINUX}
      {$DEFINE V_WIN32}
    {$ENDIF}
    {$IFNDEF VER90}         { Delphi 2.0 }
      {$IFNDEF VER93}       { C++ Builder 1.0 }
        {$DEFINE V_D3}      { Delphi 3.0 or higher }
        {$DEFINE V_INTERFACE}
        {$DEFINE V_FREEMEM_NIL}
        {$DEFINE MSWINDOWS}
        {$IFNDEF VER100}           { Delphi 3.0 }
          {$IFNDEF VER110}         { C++ Builder 3.0 }
            {$DEFINE V_D4}         { Delphi 4.0 or higher }
            {$UNDEF INT64_EQ_COMP}
            {$IFNDEF VER120}       { Delphi 4.0 }
            {$IFNDEF VER125}       { C++ Builder 4.0 }
              {$DEFINE V_D5}       { Delphi 5.0 or higher }
              {$IFNDEF VER130}     { Delphi 5.0 / C++ Builder 5.0 }
                {$DEFINE V_D6}     { Delphi 6.0 or higher }
                  {$IFNDEF VER140} { Delphi 6.0 }
                    {$IFNDEF KYLIX}
                      {$DEFINE V_D7} { Delphi 7.0 or higher }
                      {$WARN UNSAFE_CAST OFF}
                      {$WARN UNSAFE_CODE OFF}
                      {$WARN UNSAFE_TYPE OFF}
                      {$IFNDEF VER150}
                        {$DEFINE V_D8} { Delphi 8.0 or higher }
                        {$IFNDEF VER160}
                          {$DEFINE V_D2005} { Delphi 2005 or higher }
                          {.$DEFINE V_INLINE}
                          { there are mysterious error when inlining is on under
                            Delphi 2005: access violations and wrong calculation
                            results (!) so disable inilining }
                        {$ENDIF}
                      {$ENDIF}
                    {$ENDIF}
                  {$ENDIF}
              {$ENDIF}
            {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
    {$DEFINE V_16}          { Delphi 1.0: 16-bit environment }
  {$ENDIF}
{$ENDIF}
{
  �������� �������� �� ����������� �������� ��� ���������� ������ ��������� (���
  ������ ����� ��������� ����������, ����� ������������ � ����������� �������
  �������� ��������� ����������� �������� � ����������� �������� �� ������
  CheckFree); ���� �����-���� ������� �� ����� ����������, ��, ��-������, �����
  ������ ��������������, �, ��-������, � ���� "free_err.log", ����������� �
  ����������, �� ������� ���� �������� ���������, ����� �������� ����� �������
  �������������� ��������.
  Enable run-time checks for correct object destruction (these checks will work
  if class constructors and destructors call special registration procedures
  from the CheckFree unit); if some objects will not be freed then finalization
  code of CheckFree will show warning message and write class names of unfreed
  objects to file "free_err.log", residing in the same directory as the application.
}
{$IFDEF V_DELPHI}
  {$IFDEF VDEBUG}
    {$DEFINE CHECK_OBJECTS_FREE}
  {$ENDIF}
  {$IFDEF V_32}
    { ��������� "�����������" �������������� (warnings) }
    { turns off "checked" compiler warnings }
    {$DEFINE NOWARN}
    { ��������� "�����������" ��������� (hints) }
    { turns off "checked" compiler hints }
    {$DEFINE NOHINTS}
  {$ENDIF}
{$ENDIF}
{$IFNDEF BCB}
  {$IFDEF VER93} {$DEFINE BCB} {$ENDIF}
  {$IFDEF VER110} {$DEFINE BCB} {$ENDIF}
  {$IFDEF VER125} {$DEFINE BCB} {$ENDIF}
{$ENDIF}
{
  ���������: ������ �������������� ������ i386.
  Platform: only i386 is now supported.
}
{$DEFINE V_PLATFORM_I386}
{
  USE_ASM � USE_MMX - ������ "���������" ��������� ������������� ������������
  ������� � MMX-������ ��������������; ������ ������ ���� ���������� �����������
  �� ����������, �.�. ��� ������������� ���������, ��� ����������� �����������
  ������ ���������� (R-, Q-) ��� ��������� ������������ MMX.
  USE_ASM and USE_MMX are only "requests" for using assembler fragments and
  MMX instructions; the library units must check that appropriate compiler options
  (R-, Q-) are used and that the processor supports MMX.
}
{$IFDEF V_PLATFORM_I386}
  {$IFDEF V_32}
    {$IFDEF V_FREEPASCAL}
      {$ASMMODE INTEL}
    {$ENDIF}
    {$IFNDEF CLR}
      {$DEFINE USE_ASM}
      {$DEFINE USE_MMX}
      {$DEFINE USE_3DNow}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$IFDEF LINUX}
  {$DEFINE V_CASE_SENS_FILENAMES}
{$ENDIF}
{
  ��������� ������������� ������� ����� (AnsiString).
  Enable usage of long strings (AnsiString).
}
{$IFDEF V_32}
  {$DEFINE V_LONGSTRINGS}
  {$LONGSTRINGS ON}
  {$IFDEF V_DELPHI}
    {$DEFINE V_AUTOINITSTRINGS}
  {$ENDIF}
{$ENDIF}
{
  ��������� ������������� Unicode ����� (WideString) � ���������� �� ���������.
  Enable usage of Unicode strings (WideString) and default parameters.
}
{$IFDEF V_32}
  {$DEFINE V_WIDESTRINGS}
  {$IFDEF V_D4}
    {$DEFINE V_WIDESTRING_PLUS}
    {$DEFINE V_DEFAULTS}
  {$ENDIF}
{$ENDIF}
{
  ��������� ������������� Unicode-���� ������ � �������.
  Enable usage of Unicode file name in streams.
}
{$IFDEF WIN32}
  {$IFNDEF V_FREEPASCAL}
    {$DEFINE W_STREAM}
    {$DEFINE W_STREAM_WIN}
  {$ENDIF}
{$ENDIF}
{$IFDEF V_KYLIX}
  {$DEFINE W_STREAM}
{$ENDIF}
