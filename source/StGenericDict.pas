unit StGenericDict;

interface

uses
  Windows, SysUtils, Classes, StBase, StConst, StGenericBase, System.Generics.Defaults, System.Generics.Collections;

type
  TStDictNode<TKey,TData> = class(TStNode<TData>)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;
  protected
    dnNext : TStDictNode<TKey,TData>;
    FKey : TKey;
  public
    constructor create(const AKey: TKey; const AData: TData);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Key : TKey read FKey;
    property Name : TKey read FKey;
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;

  TStDictionary<TKey,TData> = class sealed(TStContainer<TData>)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;
    class var ClassCritSect : TRTLCriticalSection;
  public
    type
      TSymbolArray = array[0..(StMaxBlockSize div sizeof(Pointer))-1] of TStDictNode<TKey,TData>;
      PSymbolArray = ^TSymbolArray;
      TIterateFunc<TOtherData> = function(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : TOtherData) : Boolean;
      TOnIterateFunc<TOtherData> = function(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : TOtherData) : Boolean of object;
      TPairData = TPair<TKey,TData>;
  private
    type
      PExtractKeyAndData = ^TExtractKeyAndData;
      TExtractKeyAndData = record
        data: TArray<TPairData>;
        index: integer;
      end;
      PExtractKey = ^TExtractKey;
      TExtractKey = record
        data: TArray<TKey>;
        index: integer;
      end;
  public
    class procedure EnterClassCS;
    class procedure LeaveClassCS;
    class function DestroyNode(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : Pointer) : Boolean;
    class function FindNodeData(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : TData) : Boolean;
    class function OnExtractKeyData(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : PExtractKeyAndData) : Boolean;
    class function OnExtractKey(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : PExtractKey) : Boolean;
  protected
    FOnDisposeKey : TStDisposeEvent<TKey>;
    FDisposeKey : TDisposeProc<TKey>;
    FKeyComparer: System.Generics.Defaults.IEqualityComparer<TKey>;
    FDataComparer: System.Generics.Defaults.IEqualityComparer<TData>;
    FHashSize : Integer;
    dySymbols : PSymbolArray;
    dyIgnoreDups : Boolean;
    procedure dySetHashSize(Size : Integer);
    procedure dyFindNode(const Name : TKey; var H : Integer; var Prev, This : TStDictNode<TKey,TData>);
    procedure DisposeNodeKey(const P: TStDictNode<TKey,TData>);
  public
    class constructor create;
    class destructor Destroy;
    constructor Create(const AHashSize : Integer; const AKeyComparer: System.Generics.Defaults.IEqualityComparer<TKey>; const ADataComparer: System.Generics.Defaults.IEqualityComparer<TData>); overload;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Clear; override;
    function ExtractKeyAndData(): TArray<TPairData>;
    function ExtractKeys: TArray<TKey>;
    function Exists(const Name : TKey; var Data : TData) : Boolean; overload;
    function Exists(const Name : TKey) : Boolean; overload;
    procedure Add(const Name : TKey; Data : TData);
    procedure Delete(const Name : TKey);
    function Find(Data : TData; var Name : TKey) : Boolean;
    function Iterate<TOtherData>(Action : TIterateFunc<TOtherData>; OtherData : TOtherData) : TStDictNode<TKey,TData>;
    function OnIterate<TOtherData>(Action : TOnIterateFunc<TOtherData>; OtherData : TOtherData) : TStDictNode<TKey,TData>;
    function BinCount(H : Integer) : Integer;
    procedure DoDisposeKey(const AKey : TKey); virtual;
    property HashSize : Integer read FHashSize;
    property DisposeKey : TDisposeProc<TKey> read FDisposeKey write FDisposeKey;
    property OnDisposeKey : TStDisposeEvent<TKey> read FOnDisposeKey write FOnDisposeKey;
    property KeyComparer: System.Generics.Defaults.IEqualityComparer<TKey> read FKeyComparer write FKeyComparer;
    property DataComparer: System.Generics.Defaults.IEqualityComparer<TData> read FDataComparer;
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;

  TDefinedDictionaries = class
  public
    class function CreateCaseInsensitiveAnsiStringDictionary<TData>(const ABinSize: integer) : TStDictionary<AnsiString,TData>;
    class function CreateCaseSensitiveAnsiStringDictionary<TData>(const ABinSize: integer) : TStDictionary<AnsiString,TData>;
  end;

  TAnsiStringCaseInsensitiveEqualityComparerImpl = class(TInterfacedObject, IEqualityComparer<AnsiString>)
  protected
    function Equals(const Left, Right: AnsiString): Boolean; reintroduce;
    function GetHashCode(const Value: AnsiString): Integer; reintroduce;
  end;

  TStringCaseInsensitiveEqualityComparerImpl = class(TInterfacedObject, IEqualityComparer<String>)
  protected
    function Equals(const Left, Right: String): Boolean; reintroduce;
    function GetHashCode(const Value: String): Integer; reintroduce;
  end;

  TShortStringCaseInsensitiveEqualityComparerImpl = class(TInterfacedObject, IEqualityComparer<ShortString>)
  protected
    function Equals(const Left, Right: ShortString): Boolean; reintroduce;
    function GetHashCode(const Value: ShortString): Integer; reintroduce;
  end;

  TAnsiStringCaseSensitiveEqualityComparerImpl = class(TInterfacedObject, IEqualityComparer<AnsiString>)
  protected
    function Equals(const Left, Right: AnsiString): Boolean; reintroduce;
    function GetHashCode(const Value: AnsiString): Integer; reintroduce;
  end;

  TStringCaseSensitiveEqualityComparerImpl = class(TInterfacedObject, IEqualityComparer<String>)
  protected
    function Equals(const Left, Right: String): Boolean; reintroduce;
    function GetHashCode(const Value: String): Integer; reintroduce;
  end;


implementation

uses
  AnsiStrings, System.Hash;

class constructor TStDictionary<TKey,TData>.create;
begin
  Windows.InitializeCriticalSection(ClassCritSect);
end;

class destructor TStDictionary<TKey,TData>.Destroy;
begin
  Windows.DeleteCriticalSection(ClassCritSect);
end;

class function TStDictionary<TKey,TData>.DestroyNode(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : Pointer) : Boolean;
begin
  Container.DisposeNodeData(Node);
  Container.DisposeNodeKey(Node);
  Node.Free;
  Result := True;
end;

procedure TStDictionary<TKey, TData>.DisposeNodeKey(const P: TStDictNode<TKey,TData>);
begin
  EnterCS;
  try
    if Assigned(P) then
    begin
      DoDisposeKey(P.Key);
    end;
  finally
    LeaveCS;
  end;
end;

procedure TStDictionary<TKey, TData>.DoDisposeKey(const AKey : TKey);
begin
  if Assigned(FOnDisposeKey) then
  begin
    FOnDisposeKey(Self, AKey)
  end
  else if Assigned(FDisposeKey) then
  begin
    FDisposeKey(AKey);
  end;
end;

class procedure TStDictionary<TKey,TData>.EnterClassCS;
begin
  EnterCriticalSection(ClassCritSect);
end;

class procedure TStDictionary<TKey,TData>.LeaveClassCS;
begin
  LeaveCriticalSection(ClassCritSect);
end;

procedure TStDictNode<TKey, TData>.AfterConstruction;
begin
  inherited AfterConstruction;

  InterlockedIncrement(FCreateCount);
end;

procedure TStDictNode<TKey, TData>.BeforeDestruction;
begin
  InterlockedIncrement(FDestroyCount);

  inherited BeforeDestruction;
end;

destructor TStDictNode<TKey,TData>.Destroy;
begin
  inherited Destroy;
end;

constructor TStDictNode<TKey,TData>.create(const AKey: TKey; const AData: TData);
begin
  inherited create(AData);
  FKey := AKey;
end;

class function TStDictionary<TKey,TData>.FindNodeData(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : TData) : Boolean;
begin
  Result :=  not Container.FDataComparer.Equals(OtherData, Node.Data);
end;

procedure TStDictionary<TKey,TData>.Add(const Name : TKey; Data : TData);
var
  H : Integer;
  P, T : TStDictNode<TKey,TData>;
begin
  EnterCS;
  try
    dyFindNode(Name, H, P, T);
    if Assigned(T) then
      RaiseContainerError(stscDupNode);
    T := dySymbols^[H];
    dySymbols^[H] := TStDictNode<TKey,TData>.create(Name, Data);
    dySymbols^[H].dnNext := T;
    Inc(FCount);
  finally
    LeaveCS;
  end;
end;

procedure TStDictionary<TKey, TData>.AfterConstruction;
begin
  inherited AfterConstruction;

  InterlockedIncrement(FCreateCount);
end;

procedure TStDictionary<TKey, TData>.BeforeDestruction;
begin
  InterlockedIncrement(FDestroyCount);

  inherited BeforeDestruction;
end;

function TStDictionary<TKey,TData>.BinCount(H : Integer) : Integer;
var
  C : Integer;
  T : TStDictNode<TKey,TData>;
begin
  EnterCS;
  try
    C := 0;
    T := dySymbols^[H];
    while Assigned(T) do begin
      inc(C);
      T := T.dnNext;
    end;
    Result := C;
  finally
    LeaveCS;
  end;
end;


class function TStDictionary<TKey,TData>.OnExtractKeyData(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : PExtractKeyAndData) : Boolean;
begin
  OtherData^.data[OtherData^.index] := TPairData.Create(Node.Key, Node.Data);
  inc(OtherData^.index);

  Result := true;
end;

class function TStDictionary<TKey,TData>.OnExtractKey(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : PExtractKey) : Boolean;
begin
  OtherData^.data[OtherData^.index] := Node.Key;
  inc(OtherData^.index);

  Result := true;
end;


function TStDictionary<TKey,TData>.ExtractKeyAndData(): TArray<TPairData>;
begin
  Lock;
  try
    SetLength(Result, Count);
    var LIV: TExtractKeyAndData;
    fillchar(LIV, sizeof(LIV), 0);
    LIV.data := Result;
    LIV.index := 0;
    OnIterate<PExtractKeyAndData>(OnExtractKeyData, @LIV);
  finally
    Unlock;
  end;
end;

function TStDictionary<TKey,TData>.ExtractKeys(): TArray<TKey>;
begin
  Lock;
  try
    SetLength(Result, Count);
    var LIV: TExtractKey;
    fillchar(LIV, sizeof(LIV), 0);
    LIV.data := Result;
    LIV.index := 0;
    OnIterate<PExtractKey>(OnExtractKey, @LIV);
  finally
    Unlock;
  end;
end;


procedure TStDictionary<TKey,TData>.Clear;
begin
  EnterCS;
  try
    if FCount <> 0 then begin
      OnIterate<Pointer>(DestroyNode, nil);
      FCount := 0;
      FillChar(dySymbols^, Integer(FHashSize)*SizeOf(TStDictNode<TKey,TData>), 0);
    end;
  finally
    LeaveCS;
  end;
end;

constructor TStDictionary<TKey, TData>.create(
  const AHashSize: Integer;
  const AKeyComparer: System.Generics.Defaults.IEqualityComparer<TKey>;
  const ADataComparer: System.Generics.Defaults.IEqualityComparer<TData>);
begin
  inherited Create;
  var LKeyComparer: System.Generics.Defaults.IEqualityComparer<TKey> := AKeyComparer;
  if not assigned(LKeyComparer) then
  begin
    LKeyComparer := System.Generics.Defaults.IEqualityComparer<TKey>(TEqualityComparer<TKey>._Default);
  end;

  var LDataComparer: System.Generics.Defaults.IEqualityComparer<TData> := ADataComparer;
  if not assigned(LDataComparer) then
  begin
    LDataComparer := System.Generics.Defaults.IEqualityComparer<TData>(TEqualityComparer<TData>._Default);
  end;

  FKeyComparer := LKeyComparer;
  FDataComparer := LDataComparer;
  dySetHashSize(AHashSize)
end;

procedure TStDictionary<TKey,TData>.Delete(const Name : TKey);
var
  H : Integer;
  P, T : TStDictNode<TKey,TData>;
begin
  EnterCS;
  try
    dyFindNode(Name, H, P, T);
    if Assigned(T) then begin
      if Assigned(P) then
        P.dnNext := T.dnNext
      else
        dySymbols^[H] := T.dnNext;
      DestroyNode(Self, T, nil);
      Dec(FCount);
    end;
  finally
    LeaveCS;
  end;
end;

destructor TStDictionary<TKey,TData>.Destroy;
begin
  if conNodeProt = 0 then
    Clear;
  if Assigned(dySymbols) then
    FreeMem(dySymbols, Integer(FHashSize)*SizeOf(TStDictNode<TKey,TData>));
  IncNodeProtection;
  inherited Destroy;
end;

procedure TStDictionary<TKey,TData>.dyFindNode(const Name : TKey; var H : Integer; var Prev, This : TStDictNode<TKey,TData>);
var
  P, T : TStDictNode<TKey,TData>;
begin
  Prev := nil;
  This := nil;
  H := (FKeyComparer.GetHashCode(Name) and $7FFFFFFF mod FHashSize) and $7FFFFFFF;
  T := dySymbols^[H];
  P := nil;
  while Assigned(T) do begin
    if FKeyComparer.Equals(Name, T.Key) then
    begin
      Prev := P;
      This := T;
      Exit;
    end;
    P := T;
    T := T.dnNext;
  end;

  {Not found}
  This := nil;
end;

procedure TStDictionary<TKey,TData>.dySetHashSize(Size : Integer);
var
  TableSize : Integer;
begin
  EnterCS;
  try
    TableSize := Integer(Size)*SizeOf(TStDictNode<TKey,TData>);
    if (Size <= 0) {or (TableSize > MaxBlockSize)} then
    begin
      RaiseContainerError(stscBadSize);
    end;

    if Size <> FHashSize then
    begin
      GetMem(dySymbols, TableSize);
      FillChar(dySymbols^, TableSize, 0);
      FCount := 0;
      FHashSize := Size;
    end;
  finally
    LeaveCS;
  end;
end;

function TStDictionary<TKey,TData>.Exists(const Name : TKey) : Boolean;
begin
  var LData: TData := default(TData);
  Result := Exists(Name, LData);
end;

function TStDictionary<TKey,TData>.Exists(const Name : TKey; var Data : TData) : Boolean;
var
  H : Integer;
  P, T : TStDictNode<TKey,TData>;
begin
  EnterCS;
  try
    dyFindNode(Name, H, P, T);
    if Assigned(T) then begin
      if Assigned(P) then begin
        {Move T to front of list}
        P.dnNext := T.dnNext;
        T.dnNext := dySymbols^[H];
        dySymbols^[H] := T;
      end;
      Result := True;
      Data := T.Data;
    end else
      Result := False;
  finally
    LeaveCS;
  end;
end;

function TStDictionary<TKey,TData>.Find(Data : TData; var Name : TKey) : Boolean;
var
  T : TStDictNode<TKey,TData>;
begin
  Name := default(TKey);
  EnterCS;
  try
    T := OnIterate<TData>(FindNodeData, Data);
    if Assigned(T) then begin
      Result := True;
      Name := T.Key;
    end else
      Result := False;
  finally
    LeaveCS;
  end;
end;

function TStDictionary<TKey,TData>.Iterate<TOtherData>(Action : TIterateFunc<TOtherData>; OtherData : TOtherData) : TStDictNode<TKey,TData>;
var
  H    : Integer;
  T, N : TStDictNode<TKey,TData>;
begin
  EnterCS;
  try
    if FCount <> 0 then begin
      for H := 0 to FHashSize-1 do begin
        T := dySymbols^[H];
        while Assigned(T) do begin
          N := T.dnNext;
          if Action(Self, T, OtherData) then
            T := N
          else begin
            Result := T;
            Exit;
          end;
        end;
      end;
    end;
    Result := nil;
  finally
    LeaveCS;
  end;
end;

function TStDictionary<TKey,TData>.OnIterate<TOtherData>(Action : TOnIterateFunc<TOtherData>; OtherData : TOtherData) : TStDictNode<TKey,TData>;
var
  H    : Integer;
  T, N : TStDictNode<TKey,TData>;
begin
  EnterCS;
  try
    if FCount <> 0 then begin
      for H := 0 to FHashSize-1 do begin
        T := dySymbols^[H];
        while Assigned(T) do begin
          N := T.dnNext;
          if Action(Self, T, OtherData) then
            T := N
          else begin
            Result := T;
            Exit;
          end;
        end;
      end;
    end;
    Result := nil;
  finally
    LeaveCS;
  end;
end;



{ TDefinedDictionaries }

class function TDefinedDictionaries.CreateCaseInsensitiveAnsiStringDictionary<TData>(const ABinSize: integer): TStDictionary<AnsiString, TData>;
begin
  Result := TStDictionary<AnsiString,TData>.Create(ABinSize, nil, nil);
  Result.KeyComparer := TAnsiStringCaseInsensitiveEqualityComparerImpl.Create;
end;

{ TAnsiStringCaseInsensitiveEqualityComparerImpl }


class function TDefinedDictionaries.CreateCaseSensitiveAnsiStringDictionary<TData>(
  const ABinSize: integer): TStDictionary<AnsiString, TData>;
begin
  Result := TStDictionary<AnsiString,TData>.Create(ABinSize, nil, nil);
  Result.KeyComparer := TAnsiStringCaseSensitiveEqualityComparerImpl.Create;
end;

{ TAnsiStringCaseInsensitiveEqualityComparerImpl }

function TAnsiStringCaseInsensitiveEqualityComparerImpl.Equals(const Left, Right: AnsiString): Boolean;
begin
  Result := Ansistrings.AnsiSameText(Left, Right)
end;

function TAnsiStringCaseInsensitiveEqualityComparerImpl.GetHashCode(const Value: AnsiString): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Ansistrings.UpperCase(Value));
end;


{ TAnsiStringCaseSensitiveEqualityComparerImpl }

function TAnsiStringCaseSensitiveEqualityComparerImpl.Equals(const Left, Right: AnsiString): Boolean;
begin
  Result := Ansistrings.AnsiSameStr(Left, Right)
end;

function TAnsiStringCaseSensitiveEqualityComparerImpl.GetHashCode(const Value: AnsiString): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Value);
end;

{ TShortStringCaseInsensitiveEqualityComparerImpl }

function TShortStringCaseInsensitiveEqualityComparerImpl.Equals(const Left, Right: ShortString): Boolean;
begin
  Result := Ansistrings.AnsiSameText(Left, Right)
end;

function TShortStringCaseInsensitiveEqualityComparerImpl.GetHashCode(const Value: ShortString): Integer;
begin
  Result := THashFNV1a32.GetHashValue(Ansistrings.UpperCase(Value));
end;

{ TStringCaseInsensitiveEqualityComparerImpl }

function TStringCaseInsensitiveEqualityComparerImpl.Equals(const Left, Right: String): Boolean;
begin
  Result := SysUtils.AnsiSameText(Left, Right)
end;

function TStringCaseInsensitiveEqualityComparerImpl.GetHashCode(const Value: String): Integer;
begin
  Result :=  THashBobJenkins.GetHashValue(SysUtils.UpperCase(Value));
end;

{ TStringCaseSensitiveEqualityComparerImpl }

function TStringCaseSensitiveEqualityComparerImpl.Equals(const Left, Right: String): Boolean;
begin
  Result := SysUtils.AnsiSameStr(Left, Right)
end;

function TStringCaseSensitiveEqualityComparerImpl.GetHashCode(const Value: String): Integer;
begin
  Result :=  THashBobJenkins.GetHashValue(SysUtils.UpperCase(Value));
end;

end.
