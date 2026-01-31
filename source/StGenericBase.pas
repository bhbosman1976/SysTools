unit StGenericBase;

interface

uses
  Windows, Classes, SysUtils, Messages, StdCtrls, StConst, stBase;

type
  TStNode<TData> = class(TObject)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;
  protected
    FData : TData;
  public
    constructor Create(const AData: TData);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
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
    procedure Lock;
    procedure Unlock;
    function TryLock: boolean;

    property Count : Integer read FCount;
    property Compare : TCompareFunc<TData> read FCompare write FCompare;
    property DisposeData : TDisposeProc<TData> read FDisposeData write FDisposeData;
    property OnCompare : TStCompareEvent<TData> read FOnCompare write FOnCompare;
    property OnDisposeData : TStDisposeEvent<TData> read FOnDisposeData write FOnDisposeData;
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;


implementation

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

constructor TStNode<TData>.Create(const AData: TData);
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


procedure TStContainer<TData>.Lock;
begin
  EnterCS;
end;

procedure TStContainer<TData>.Unlock;
begin
  LeaveCS;
end;


function TStContainer<TData>.TryLock: boolean;
begin
  Result := TryEnterCriticalSection(conThreadSafe);
end;

end.


