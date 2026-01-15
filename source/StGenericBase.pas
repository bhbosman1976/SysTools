// Upgraded to Delphi 2009: Sebastian Zierer

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* SysTools: StBase.pas 4.04                             *}
{*********************************************************}
{* SysTools: Base unit for SysTools                      *}
{*********************************************************}

{$I StDefine.inc}

unit StGenericBase;


interface

uses
  Windows, Classes, SysUtils, Messages, StdCtrls, StConst;

const
  StMaxBlockSize = MaxLongInt;

type
{!!.01 - moved from StBase.pas }
  TStLineTerminator = ( {possible line terminators...}
     ltNone,            {..no terminator, ie fixed length lines}
     ltCR,              {..carriage return (#13)}
     ltLF,              {..line feed (#10)}
     ltCRLF,            {..carriage return/line feed (#13/#10)}
     ltOther);          {..another character}
{!!.01 - end moved }

type
  TStHwnd = HWND;

type
  EStException = class(Exception)     {ancestor to all SysTools exceptions}
  protected
    FErrorCode : Integer;
  public
    constructor CreateResTP(Ident : Integer; Dummy : Word);
    constructor CreateResFmtTP(Ident : Integer; const Args : array of const; Dummy : Word);
    property ErrorCode : Integer read FErrorCode write FErrorCode;
  end;
  EStExceptionClass = class of EStException;

  EStContainerError = class(EStException);   {container exceptions}
  EStSortError = class(EStException);        {sorting exceptions}
  EStRegIniError = class(EStException);      {registry/INI file exceptions}
  EStBCDError = class(EStException);         {Bcd exceptions}
  EStStringError = class(EStException);      {String class exceptions}
  EStVersionInfoError = class(EStException); {Version info exception}
  EStNetException = class(EStException);     {Network exception}
  EStBarCodeError = class(EStException);     {BarCode exception}
  EStPNBarCodeError = class(EStException);   {PostNet BarCode exception}
  EStStatError = class(EStException);        {statistics exceptions}
  EStFinError = class(EStException);         {Financial exceptions}
  EStMimeError = class(EStException);        {Mime exceptions}
  EStToHTMLError = class(EStException);      {ToHTML exceptions}
  EStSpawnError = class(EStException);       {SpawnApplication errors}
  EStMMFileError = class(EStException);      {MemoryMappedFile errors}
  EStBufStreamError =class(EStException);    {Buffered stream errors}
  EStRegExError = class(EStException);       {RegEx errors}
  EStDecMathError = class(EStException);     {Decimal math errors}
  EStPRNGError = class(EStException);        {Random number errors}

  EStExprError = class(EStException) {expression evaluator exceptions}
  protected
    FErrorCol : Integer;
  public
    constructor CreateResTPCol(Ident : Integer; Column : Integer; Dummy : Integer);
    property ErrorColumn : Integer read FErrorCol;
  end;

const
{.Z+}
  StMaxFileLen  = 260;

  StRLEMaxCount = 127;      { Used by RLE }
  StRLERunMode = $80;       { Used by RLE }
{.Z-}

const
{.Z+}
  StHexDigitsA  : array[0..$F] of AnsiChar = '0123456789ABCDEF';
  StHexDigits  : array[0..$F] of Char = '0123456789ABCDEF';
  DosDelimSet  : set of AnsiChar = ['\', ':', #0];
  StHexDigitsW : WideString = '0123456789ABCDEF';
  DosDelimSetW : WideString = '\:';

{.Z-}

type
{.Z+}
  TSmallArrayA = array[0..StMaxFileLen-1] of AnsiChar;
  TSmallArray = array[0..StMaxFileLen-1] of Char;
  BTable  = array[0..255] of Byte;  {Table used by Boyer-Moore search routines}
  BTableU = array[0..$FFFF] of Byte;
{.Z-}

type
{.Z+}
  PDouble = ^Double;
  TDoubleArray = array[0..(stMaxBlockSize div SizeOf(Double))-1] of Double;
  PDoubleArray = ^TDoubleArray;
  TIntArray = array[0..(StMaxBlockSize div SizeOf(Integer))-1] of Integer;
  PIntArray = ^TIntArray;
{.Z-}

type
    TStFloat = Extended;

const
  WMCOPYID : DWORD = $AFAF;

type
  TStNode<TData> = class(TObject)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;
  protected
    FData : TData;
  public
    constructor Create; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Init(const AData: TData); virtual;
    property Data : TData read FData write FData;
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;

  TCompareFunc<TData> = function(Data1, Data2 : TData) : Integer;
  TStCompareEvent<TData> = procedure(Sender : TObject; Data1, Data2 : TData; var Compare : Integer) of object;
  TDisposeProc<TData> = procedure(Data : TData);
  TStDisposeEvent<TData> = procedure(Sender : TObject; Data : TData) of object;

  TStContainer<TData> = class(TObject)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;
  protected
    FCompare     : TCompareFunc<TData>;
    FDisposeData : TDisposeProc<TData>;
    FOnCompare     : TStCompareEvent<TData>;
    FOnDisposeData : TStDisposeEvent<TData>;
    conThreadSafe  : TRTLCriticalSection;
  protected
    conNodeProt  : Integer;
    FCount       : Integer;
    procedure IncNodeProtection;
    procedure DecNodeProtection;
    procedure EnterCS;
    procedure LeaveCS;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Clear; virtual; abstract;
    procedure DisposeNodeData(P : TStNode<TData>);
    procedure DoDisposeData(Data : TData); virtual;
    property Count : Integer read FCount;
    property Compare : TCompareFunc<TData> read FCompare write FCompare;
    property DisposeData : TDisposeProc<TData> read FDisposeData write FDisposeData;
    property OnCompare : TStCompareEvent<TData> read FOnCompare write FOnCompare;
    property OnDisposeData : TStDisposeEvent<TData> read FOnDisposeData write FOnDisposeData;
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;

  TAssignRowData = record
    RowNum : Integer;
    Data   : array [0..0] of Byte;
  end;

  {.Z+}
  { base component for SysTools non-visual components}
  TStComponent = class(TComponent)
  protected {private}
    function GetVersion : string;
    procedure SetVersion(const Value : string);

  published
    property Version : string
      read GetVersion
      write SetVersion
      stored False;
  end;

  { base component for TStExpressionEdit component }
  TStBaseEdit = class(TEdit)
  protected {private}
    function GetVersion : string;
    procedure SetVersion(const Value : string);

  published
    property Version : string
      read GetVersion
      write SetVersion
      stored False;
  end;
  {.Z-}

  {-Generic function to pass to iterator to destroy a container node}


{---WIN32 short string routines---}

{.Z+}
{---Huge memory routines---}
procedure HugeFillStruc(var ADest; ADestSize: Integer; const ASource; ASourceSize: Integer);
  {-Fill huge memory block with structure value}

procedure HugeFreeMem(var P : Pointer; Size : Integer);
  {-Free huge memory block allocation}
{.Z-}


{---Miscellaneous---}

{.Z+}
function IsOrInheritsFrom(Root, Candidate : TClass) : boolean;
  {-Return true if the classes are equal or Candidate is a descendant of Root}

procedure RaiseContainerError(Code : Integer);
  {-Internal routine: raise an exception for a container}

procedure RaiseContainerErrorFmt(Code : Integer; Data : array of const);
  {-Internal routine: raise an exception for a container}

function ProductOverflow(A, B : Integer) : Boolean;
  {-Return True if A*B exceeds MaxLongInt}

{.Z-}


{---primitives for converting strings to integers}
procedure ValLongInt(S :string; var LI : Integer; var ErrorCode : integer);
procedure ValSmallint(const S : string; var SI : smallint; var ErrorCode : integer);
procedure ValWord(const S : string; var Wd : word; var ErrorCode : integer);

{.Z+}
{general routine to raise a specific class of SysTools exception}
procedure RaiseStError(ExceptionClass : EStExceptionClass; Code : Integer);
{.Z-}

{.Z+}
{general routines to raise a specific Win32 exception in SysTools}
procedure RaiseStWin32Error(ExceptionClass : EStExceptionClass; Code : Integer);
procedure RaiseStWin32ErrorEx(ExceptionClass : EStExceptionClass; Code : Integer; Info : string);
{.Z-}

implementation

uses
  Math, Character;

procedure RaiseStError(ExceptionClass : EStExceptionClass; Code : Integer);
var
  E : EStException;
begin
  E := ExceptionClass.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseStWin32Error(ExceptionClass : EStExceptionClass; Code : Integer);
var
  E : EStException;
begin
  E := ExceptionClass.Create(SysErrorMessage(Code));
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseStWin32ErrorEx(ExceptionClass : EStExceptionClass; Code : Integer;
          Info : string);
var
  E : EStException;
begin
  E := ExceptionClass.Create(SysErrorMessage(Code) + ' [' + Info + ']');
  E.ErrorCode := Code;
  raise E;
end;

constructor EStException.CreateResTP(Ident : Integer; Dummy : Word);
begin
  inherited Create(SysToolsStr(Ident));
end;

constructor EStException.CreateResFmtTP(Ident : Integer;
            const Args : array of const; Dummy : Word);
begin
  inherited CreateFmt(SysToolsStr(Ident), Args);
end;

constructor EStExprError.CreateResTPCol(Ident : Integer; Column : Integer; Dummy : Integer);
begin
  inherited CreateResTP(Ident, 0);

  FErrorCol := Column;
end;




procedure HugeFillStruc(var ADest; ADestSize: Integer; const ASource; ASourceSize: Integer);
var
  iSize: Integer;
  pDest: Pointer;
begin
  pDest := @ADest;
  iSize := Min(ADestSize, ASourceSize);
  while iSize > 0 do
  begin
    Move(ASource, pDest^, iSize);
    Inc(NativeInt(pDest), iSize);
    ADestSize := ADestSize - iSize;
    iSize := Min(ADestSize, ASourceSize);
  end;
end;

procedure HugeFreeMem(var P : Pointer; Size : Integer);
begin
  if Assigned(P) then begin
    FreeMem(P, Size);
    P := nil;
  end;
end;

function ProductOverflow(A, B : Integer) : Boolean;
register;
asm
  mov ecx,False
  {A is in eax already, B is in edx already}
  imul eax,edx
  jno @1
  mov ecx,True
@1:
  mov eax,ecx
end;


{---primitives for converting strings to integers---}
procedure ValLongInt(S : string; var LI : Integer; var ErrorCode : integer);
var
  LenS   : Integer;
  Offset : Integer;
  NBCInx : Integer;
begin
  LenS := S.Length;
  {trim trailing spaces}
  while (LenS > 0) and (S[LenS] = ' ') do
    dec(LenS);
  {empty strings are invalid}
  if (LenS = 0) then begin
    LI := 0;
    ErrorCode := -1;
    Exit;
  end;
  {from now on S must have at least one non-blank char}

  {find the first non-blank char}
  NBCInx := 1;
  while (S[NBCInx] = ' ') do
    inc(NBCInx);

  {check for a string of the form nnnnH}
  Offset := 0;
  if S[LenS].ToUpper = 'H' then begin
    {if the first non-blank char is the final character, then the
     string is just of the form <spaces>H and is invalid}
    if (NBCInx = LenS) then begin
      LI := 0;
      ErrorCode := LenS;
      Exit;
    end;
    Move(S[NBCInx], S[NBCInx+1], LenS-NBCInx);
    S[NBCInx] := '$';
    Offset := -1;
  end
  {check for a string of the form 0Xnnnn}
  else begin
    if (NBCInx < LenS) and
       (S[NBCInx] = '0') and (S[NBCInx+1].ToUpper = 'X') then begin
      S[NBCInx] := ' ';
      S[NBCInx+1] := '$';
    end;
  end;
  Val(S, LI, ErrorCode);
  if (ErrorCode <> 0) then begin
    LI := 0;
    Inc(ErrorCode, Offset);
  end;
end;

procedure ValSmallint(const S : string; var SI : smallint; var ErrorCode : integer);
const
  SmallestInt16 = -32767;
  LargestInt16 = 32767;
var
  LI : Integer;
begin
  ValLongInt(S, LI, ErrorCode);
  if (ErrorCode <> 0) then
    SI := 0
  else {the conversion succeeded} begin
    if (SmallestInt16 <= LI) and (LI <= LargestInt16) then
      SI := LI
    else begin
      ErrorCode := length(S);
      SI := 0;
    end;
  end;
end;

procedure ValWord(const S : string; var Wd : word; var ErrorCode : integer);
const
  SmallestWord = 0;
  LargestWord = 65535;
var
  LI : Integer;
begin
  ValLongInt(S, LI, ErrorCode);
  if (ErrorCode <> 0) then
    Wd := 0
  else {the conversion succeeded} begin
    if (SmallestWord <= LI) and (LI <= LargestWord) then
      Wd := LI
    else begin
      ErrorCode := length(S);
      Wd := 0;
    end;
  end;
end;
{---------------------------------------------------}


function IsOrInheritsFrom(Root, Candidate : TClass) : boolean;
  begin
    Result := (Root = Candidate) or Candidate.InheritsFrom(Root);
  end;

procedure RaiseContainerError(Code : Integer);
var
  E : ESTContainerError;
begin
  E := ESTContainerError.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseContainerErrorFmt(Code : Integer; Data : array of const);
var
  E : ESTContainerError;
begin
  E := ESTContainerError.CreateResFmtTP(Code, Data, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure TStNode<TData>.AfterConstruction;
begin
  inherited AfterConstruction;

  InterlockedIncrement(FCreateCount);
end;

procedure TStNode<TData>.BeforeDestruction;
begin
  InterlockedIncrement(FDestroyCount);

  inherited BeforeDestruction;
end;

{----------------------------------------------------------------------}

constructor TStNode<TData>.Create;
begin

end;

procedure TStNode<TData>.Init(const AData: TData);
begin
  FData := AData;
end;


procedure TStContainer<TData>.AfterConstruction;
begin
  inherited AfterConstruction;

  InterlockedIncrement(FCreateCount);
end;

procedure TStContainer<TData>.BeforeDestruction;
begin
  InterlockedIncrement(FDestroyCount);

  inherited BeforeDestruction;
end;

{----------------------------------------------------------------------}

constructor TStContainer<TData>.Create;
begin
  Windows.InitializeCriticalSection(conThreadSafe);
  inherited Create;
end;

procedure TStContainer<TData>.DecNodeProtection;
begin
  Dec(conNodeProt);
end;

destructor TStContainer<TData>.Destroy;
begin
  if conNodeProt = 0 then
    Clear;
  Windows.DeleteCriticalSection(conThreadSafe);
  inherited Destroy;
end;

procedure TStContainer<TData>.DisposeNodeData(P : TStNode<TData>);
begin
  EnterCS;
  try
    if Assigned(P) then
      DoDisposeData(P.Data);
  finally
    LeaveCS;
  end;
end;


procedure TStContainer<TData>.DoDisposeData(Data : TData);
begin
  if Assigned(FOnDisposeData) then
    FOnDisposeData(Self, Data)
  else if Assigned(FDisposeData) then
    FDisposeData(Data);
end;

procedure TStContainer<TData>.EnterCS;
begin
  EnterCriticalSection(conThreadSafe);
end;

procedure TStContainer<TData>.IncNodeProtection;
begin
  Inc(conNodeProt);
end;

procedure TStContainer<TData>.LeaveCS;
begin
  LeaveCriticalSection(conThreadSafe);
end;



{*** TStComponent ***}

function TStComponent.GetVersion : string;
begin
  Result := StVersionStr;
end;

procedure TStComponent.SetVersion(const Value : string);
begin
end;

{ TStBaseEdit }

function TStBaseEdit.GetVersion : string;
begin
  Result := StVersionStr;
end;

procedure TStBaseEdit.SetVersion(const Value : string);
begin
end;


end.


