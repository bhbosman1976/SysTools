unit StBTree;

interface

uses
  SysUtils,
  classes,
  StBase;
type
  TSCSearch = (SCSPageEmpty, SCSLessThanThisPage, SCSInThisPageRange, SCSFound, SCSGreaterThanThisPage);
  TBTreeNode<TData> = class(TObject)
  public
    const
      ArraySize = 4000 or 1;
    type
      TPageData = array[0..ArraySize-1] of TData;
        PTPageData = ^TPageData;
        TChildData = array[0..ArraySize] of Pointer;
        PTChildData = ^TChildData;
        TTreeNodeData = record
          LeafNode: boolean;
          SequentialIndex: integer;
          Count : Integer;
          PageData: TPageData;
          ChildOffset: TChildData;
    end;
  strict private
    type
      TOnTBTreeNodeDoCompare = function(Data1, Data2 : TData) : Integer of object;
      TOnDuplicates = function: boolean of object;
      TOnCreateNode = function(const ALeaf: boolean): TBTreeNode<TData> of object;
  strict private
    FTreeNodeData: TTreeNodeData;
    FOnCompare: TOnTBTreeNodeDoCompare;
    FOnDuplicates: TOnDuplicates;
    FOnCreateNode: TOnCreateNode;
    function GetChildrenPtr: PTChildData; // do not override
    function GetData: PTPageData; // do not override
    property ChildrenPtr: PTChildData read GetChildrenPtr;
  private
    FNext: TBTreeNode<TData>;
    FPrev: TBTreeNode<TData>;
    function GetLeafNode: boolean;
    function GetIsFull: boolean;
  protected
    function GetCount: integer; virtual;
    function GetSequentialIndex: integer; virtual;
    procedure SetCount(const Value: integer); virtual;
    procedure SetSequentialIndex(const Value: integer); virtual;
    function GetChild(AIndex: integer): TBTreeNode<TData>; virtual;
    procedure SetChild(AIndex: integer; const Value: TBTreeNode<TData>); virtual;
  public
    constructor Create(
      const ALeafNode: boolean;
      const AOnCompare: TOnTBTreeNodeDoCompare;
      const AOnDuplicates: TOnDuplicates;
      const AOnCreateNode: TOnCreateNode);
    destructor Destroy; override;
    procedure DoNodeAddOperation(
      const AInData : TData;
      const AInRight: TBTreeNode<TData>);
    function SplitNode(
      out ARight: TBTreeNode<TData>; 
      out AMedianData: TData): boolean;      
    function Find(const AData : TData; var PageIndex : Integer) : TSCSearch;
    property SequentialIndex: integer read GetSequentialIndex write SetSequentialIndex;
    property LeafNode: boolean read GetLeafNode;
    property Count: integer read GetCount write SetCount;
    property IsFull: boolean read GetIsFull;
    property Data: PTPageData read GetData;
    property Child[AIndex: integer]: TBTreeNode<TData> read GetChild write SetChild;
  end;

  TCustomBTree<TData> = class abstract(TObject)
  private
    type
      TAtMode = (atmProbe, atmActive);
      TTreeData = record
        FCount: integer;
      end;
  public
    type
      TOperationAnswer = (scFalse, opContinueToProcess, scCompleted);
      TNodeOperation = reference to function(
        const AActiveNode : TBTreeNode<TData>;
        const AActiveData : TData;
        const AInParam1: TBTreeNode<TData>;
        out AOutParam1: TBTreeNode<TData>;
        out AAOutParam2: TData): TOperationAnswer;
      TCompareFunc = function(Data1, Data2 : TData) : Integer;
      TStCompareEvent = procedure(Sender : TObject; Data1, Data2 : TData;  var Compare : Integer) of object;
      TCollIterateFunc = function (const AData : TData; const OtherData : Pointer; out ABTreeNode: TBTreeNode<TData>) : Boolean of object;
  strict private
    FOnCompare: TStCompareEvent;
    FCompare: TCompareFunc;
    FTreeData: TTreeData;
    FRoot: TBTreeNode<TData>;
    
    FDuplicates : Boolean;
    function DoCompare(Data1, Data2 : TData) : Integer;
    function GetDuplicates: Boolean;
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
  strict protected
    function CreateNode(const ALeaf: boolean): TBTreeNode<TData>;
    procedure UpdateNextNodeIndex(const ANode: TBTreeNode<TData>; const ACount: integer);
    function InternalOperation(
      const ANode: TBTreeNode<TData>;
      const AData: TData;
      const AOperation: TNodeOperation;
      out ARight: TBTreeNode<TData>;
      out AMedianData: TData): TOperationAnswer;
    function DoNodeAddOperation(
      const ANode : TBTreeNode<TData>;
      const AInData : TData;
      const AInRight: TBTreeNode<TData>;
      out AOutRight: TBTreeNode<TData>; out AMedianData: TData): boolean;
    function DoNodeDeleteOperation(
      const ANode : TBTreeNode<TData>;
      const AInData : TData;
      const AInRight: TBTreeNode<TData>;
      out AOutRight: TBTreeNode<TData>; out AMedianData: TData): boolean;
    function InternalIterate(
      const ANode: TBTreeNode<TData>;
      const AAction : TCollIterateFunc;
      const AUp : Boolean;
      const AOtherData : Pointer;
      out ABTreeNode: TBTreeNode<TData>;
      out AData: TData) : boolean;
    function GetInternalAt(
      const ANode: TBTreeNode<TData>;
      const AAtMode: TAtMode;
      const AIndex: integer;
      out AData: TData;
      out AIndexFoundLeft: integer;
      out AIndexFoundRight: integer;
      var ACompareCount: integer): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Insert(const AData: TData);
    procedure Delete(const AData: TData);
    function Find(const AInData: TData; out AOutData: TData): boolean;
    function Iterate(
      const Action : TCollIterateFunc;
      const Up : Boolean; OtherData : Pointer;
      out ABTreeNode: TBTreeNode<TData>) : Pointer;
    function GetAt(const AIndex: integer; out AData: Pointer): boolean; overload;
    function GetAt(const AIndex: integer; out AData: Pointer; var ACompareCount: integer): boolean; overload;
    property Duplicates : Boolean read GetDuplicates write FDuplicates;
    property Count : Integer read GetCount write SetCount;
    property OnCompare : TStCompareEvent read FOnCompare write FOnCompare;
    property Compare : TCompareFunc read FCompare write fCompare;
  end;

  TBTree<TData> = class sealed(TCustomBTree<TData>)
  protected
  end;

implementation

uses
  StConst;

constructor TBTreeNode<TData>.Create(
  const ALeafNode: boolean;
  const AOnCompare: TOnTBTreeNodeDoCompare;
  const AOnDuplicates: TOnDuplicates;
  const AOnCreateNode: TOnCreateNode);
begin
  inherited Create;

  FTreeNodeData.LeafNode := ALeafNode;
  FOnCompare := AOnCompare;
  FOnDuplicates := AOnDuplicates;
  FOnCreateNode := AOnCreateNode;
end;

destructor TBTreeNode<TData>.Destroy;
begin
  fillchar(FTreeNodeData, sizeof(FTreeNodeData), 0);
  inherited Destroy;
end;

procedure TBTreeNode<TData>.DoNodeAddOperation(
  const AInData: TData;
  const AInRight: TBTreeNode<TData>);
begin
  assert(not IsFull, 'The node is full and it cannot take more data. This should have been splitted earlier');
  var LPageIndex: integer;
  var LSearchResult := Find(AInData, LPageIndex);
  case LSearchResult of
    SCSPageEmpty, SCSInThisPageRange, SCSLessThanThisPage, SCSFound:
    begin
      if (LSearchResult = SCSFound) and not FOnDuplicates then
      begin
        RaiseContainerError(stscDupNode);
      end;
      Move(Data^[LPageIndex], Data^[LPageIndex+1], (Count-LPageIndex)*sizeof(TData));
      Move(ChildrenPtr^[LPageIndex], ChildrenPtr^[LPageIndex+1], (Count-LPageIndex+1)*sizeof(TData));

      Child[LPageIndex+1] := AInRight;
      Data^[LPageIndex] := AInData;
    end;
    SCSGreaterThanThisPage:
    begin
      Data^[Count] := AInData;
      Child[Count+1] := AInRight;
    end;
  end;
  Count := Count + 1;
end;

function TBTreeNode<TData>.GetChild(AIndex: integer): TBTreeNode<TData>;
begin
  Result := ChildrenPtr^[AIndex];
end;

function TBTreeNode<TData>.GetChildrenPtr: PTChildData;
begin
  Result := @FTreeNodeData.ChildOffset;
end;

function TBTreeNode<TData>.GetCount: integer;
begin
  Result := FTreeNodeData.Count;
end;

function TBTreeNode<TData>.GetData: PTPageData;
begin
  Result := @FTreeNodeData.PageData;
end;

function TBTreeNode<TData>.GetIsFull: boolean;
begin
   Result := Count = ArraySize;
end;

function TBTreeNode<TData>.GetSequentialIndex: integer;
begin
  Result := FTreeNodeData.SequentialIndex;
end;

function TBTreeNode<TData>.GetLeafNode: boolean;
begin
  Result := FTreeNodeData.LeafNode;
end;

procedure TBTreeNode<TData>.SetChild(AIndex: integer; const Value: TBTreeNode<TData>);
begin
  ChildrenPtr^[AIndex] := value;
end;

procedure TBTreeNode<TData>.SetCount(const Value: integer);
begin
  FTreeNodeData.Count := Value;
end;


procedure TBTreeNode<TData>.SetSequentialIndex(const Value: integer);
begin
  FTreeNodeData.SequentialIndex := Value;
end;

constructor TCustomBTree<TData>.Create;
begin
  inherited Create;

  FRoot := CreateNode(true);
end;

function TCustomBTree<TData>.CreateNode(const ALeaf: boolean): TBTreeNode<TData>;
begin
  Result := TBTreeNode<TData>.Create(ALeaf, DoCompare, GetDuplicates, createNode);
end;

procedure TCustomBTree<TData>.Delete(const AData: TData);
begin
  var LRight: TBTreeNode<TData> := nil;
  var LMedianData: TData := default(TData);
  if not (InternalOperation(
    FRoot,
    AData,
    function(const ANode : TBTreeNode<TData>; const AInData : TData; const AInRight: TBTreeNode<TData>; out AOutRight: TBTreeNode<TData>; out AMedianData: TData): TOperationAnswer
    begin
      Result := scFalse;
      var LResult := DoNodeDeleteOperation(ANode, AInData, AInRight, AOutRight, AMedianData);
      if LResult then
      begin
        // This is required for the changed node to bubble uo
        Result := opContinueToProcess;
      end;
    end,
    LRight,
    LMedianData) = scFalse) then
  begin
    var LNewRoot := CreateNode(false);
    LNewRoot.Count := 1;
    LNewRoot.Data^[0] := LMedianData;
    LNewRoot.Child[0] := FRoot;
    LNewRoot.Child[1] := LRight;
    FRoot := LNewRoot;
  end;
end;

destructor TCustomBTree<TData>.Destroy;
begin
  inherited Destroy;
end;

function TCustomBTree<TData>.DoNodeAddOperation(
  const ANode : TBTreeNode<TData>;
  const AInData : TData;
  const AInRight: TBTreeNode<TData>;
  out AOutRight: TBTreeNode<TData>; out AMedianData: TData): boolean;
begin
  ANode.DoNodeAddOperation(AInData, AInRight);
  if ANode.LeafNode then
  begin
    Count := Count + 1;
    UpdateNextNodeIndex(ANode, 1);
  end;
  Result := ANode.IsFull;
  if Result then
  begin
    // if this fails we run out of memory
     Assert(ANode.SplitNode(AOutRight, AMedianData));
  end;
end;

function TCustomBTree<TData>.DoNodeDeleteOperation(
  const ANode: TBTreeNode<TData>;
  const AInData: TData;
  const AInRight: TBTreeNode<TData>;
  out AOutRight: TBTreeNode<TData>; out AMedianData: TData): boolean;
begin
  Result := false;
end;

function TCustomBTree<TData>.Find(const AInData: TData; out AOutData: TData): boolean;
begin
  var LRight: TBTreeNode<TData> := nil;
  var LResult := InternalOperation(
    FRoot,
    AInData,
    function(
      const ANode : TBTreeNode<TData>;
      const AInData : TData;
      const notused01: TBTreeNode<TData>;
      out notused02: TBTreeNode<TData>;
      out AOutData: TData): TOperationAnswer
    begin
      Result := scfalse;
      var LPageIndex: integer;
      if ANode.Find(AInData, LPageIndex) = SCSFound then
      begin
        AOutData := ANode.Data^[LPageIndex];
        Result := scCompleted;
      end
    end,
    LRight,
    AOutData);
  case LResult of
    scFalse:
    begin
      Result := false;
    end;
  else
    begin
      Result := true;
    end;
  end;
end;

function TCustomBTree<TData>.GetAt(const AIndex: integer; out AData: Pointer; var ACompareCount: integer): boolean;
begin
  Result := false;
  if AIndex < Count then
  begin
    var LIndexFoundLeft: integer := -1;
    var LIndexFoundRight: integer := -1;
    Result := GetInternalAt(FRoot, atmActive, AIndex, AData, LIndexFoundLeft, LIndexFoundRight, ACompareCount);
  end;
end;

function TCustomBTree<TData>.GetCount: Integer;
begin
  Result :=  FTreeData.FCount;
end;

function TCustomBTree<TData>.GetAt(const AIndex: integer; out AData: Pointer): boolean;
begin
  var LCompareCount: integer := 0;
  Result := GetAt(AIndex, AData, LCompareCount)
end;

function TCustomBTree<TData>.GetDuplicates: Boolean;
begin
  Result := FDuplicates;
end;

function TCustomBTree<TData>.GetInternalAt(
  const ANode: TBTreeNode<TData>;
  const AAtMode: TAtMode;
  const AIndex: integer;
  out AData: TData;
  out AIndexFoundLeft: integer;
  out AIndexFoundRight: integer;
  var ACompareCount: integer): boolean;
var
  L: integer;
  R: integer;
  M: integer;
  LNode: TBTreeNode<TData>;
  var LLowCompare :integer;
  var LHighCompare :integer;
begin
  AIndexFoundLeft := -1;
  AIndexFoundRight := -1;
  Result := false;
  if ANode.LeafNode then
  begin
    AIndexFoundLeft := ANode.SequentialIndex;
    AIndexFoundRight := ANode.SequentialIndex + ANode.Count - 1;
    if AIndex < ANode.SequentialIndex then
    begin
      Result := false;
      inc(ACompareCount);
    end
    else if AIndex > ANode.SequentialIndex + ANode.Count -1 then
    begin
      Result := false;
      inc(ACompareCount, 2);
    end
    else
    begin
      inc(ACompareCount, 2);
      AData := ANode.Data[AIndex - ANode.SequentialIndex];
      Result := true;
    end;
  end
  else
  begin
    if AAtMode = atmProbe then
    begin
      var LTemp: integer;
      var LResultLeft := GetInternalAt(ANode.Child[0], atmProbe, AIndex, AData, AIndexFoundLeft, LTemp, ACompareCount);
      var LResultRight := GetInternalAt(ANode.Child[ANode.Count], atmProbe, AIndex, AData, LTemp, AIndexFoundRight, ACompareCount);
      Result := LResultLeft and LResultRight;
    end
    else if AAtMode = atmActive then
    begin
      L := 0;
      R := ANode.Count;
      repeat
        M := (L+R) div 2;

        LNode := ANode.Child[M];
        Result := GetInternalAt(LNode, atmProbe, AIndex, AData, AIndexFoundLeft, AIndexFoundRight, ACompareCount);
        if Result then
        begin
          exit;
        end
        else if AIndex > AIndexFoundRight then
        begin
          L := M+1;
        end
        else if AIndex < AIndexFoundLeft  then
        begin
          R := M-1;
        end
        else //if (AIndexFoundLeft <= AIndex) and (AIndex <= AIndexFoundRight) then
        begin
          break;
        end
      until L > R;

      LNode := ANode.Child[M];
      Result := GetInternalAt(LNode, atmActive, AIndex, AData, AIndexFoundLeft, AIndexFoundRight, ACompareCount);
    end;
  end;
end;

procedure TCustomBTree<TData>.Insert(const AData: TData);
begin
  var LRight: TBTreeNode<TData> := nil;
  var LMedianData: TData := default(TData);
  if not (InternalOperation(
    FRoot,
    AData,
    function(const ANode : TBTreeNode<TData>; const AInData : TData; const AInRight: TBTreeNode<TData>; out AOutRight: TBTreeNode<TData>; out AMedianData: TData): TOperationAnswer
    begin
      Result := scFalse;
      var LResult := DoNodeAddOperation(ANode, AInData, AInRight, AOutRight, AMedianData);
      if LResult then
      begin
        // This is required for the changed node to bubble uo
        Result := opContinueToProcess;
      end;
    end,
    LRight,
    LMedianData) = scFalse) then
  begin
    var LNewRoot := CreateNode(false);
    LNewRoot.Count := 1;
    LNewRoot.Data^[0] := LMedianData;
    LNewRoot.Child[0] := FRoot;
    LNewRoot.Child[1] := LRight;
    FRoot := LNewRoot;
  end;
end;

function TCustomBTree<TData>.InternalOperation(
  const ANode: TBTreeNode<TData>;
  const AData: TData;
  const AOperation: TNodeOperation;
  out ARight: TBTreeNode<TData>;
  out AMedianData: TData): TOperationAnswer;
begin
  if ANode.LeafNode then
  begin
    Result := AOperation(ANode, AData, nil, ARight, AMedianData);
  end
  else
  begin
    if DoCompare(AData, ANode.Data[0]) < 0 then
    begin
      Result := InternalOperation(
        ANode.Child[0],
        AData,
        AOperation,
        ARight,
        AMedianData);
      case Result of
        // This will bubble up the answer until it is fullfilled
        opContinueToProcess:
        begin
          Result := AOperation(ANode, AMedianData, ARight, ARight, AMedianData);
        end;
      end;
    end
    else if DoCompare(AData, ANode.Data[ANode.Count-1]) >= 0 then
    begin
      Result := InternalOperation(
        ANode.Child[ANode.Count],
        AData,
        AOperation,
        ARight,
        AMedianData);
      case Result of
        // This will bubble up the answer until it is fullfilled
        opContinueToProcess:
        begin
          Result := AOperation(ANode, AMedianData, ARight, ARight, AMedianData);
        end;
      end;
    end
    else if ANode.Count >= 2 then
    begin
      var L: integer := 0;
      var R: integer := ANode.Count-1;
      var M: integer := -1;
      repeat
        M := (L+R) div 2;
        var LCompareLow := DoCompare(AData, ANode.Data[M]);
        var LCompareHigh := DoCompare(AData, ANode.Data[M+1]);
        if ((LCompareLow >= 0) and (LCompareHigh < 0))  then
        begin
          Result := InternalOperation(
            ANode.Child[M+1],
            AData,
            AOperation,
            ARight,
            AMedianData);
          case Result of
            // This will bubble up the answer until it is fullfilled
            opContinueToProcess:
            begin
              Result := AOperation(ANode, AMedianData, ARight, ARight, AMedianData);
            end;
          end;
          exit;
        end;
        if (LCompareLow < 0)  then
        begin
          R := M-1;
        end
        else
        begin
          L := M+1
        end;
      until L > R;
      assert(false, 'This should never happen');
    end
    else
    begin
      Result := InternalOperation(
        ANode.Child[0],
        AData,
        AOperation,
        ARight,
        AMedianData);
      case Result of
        // This will bubble up the answer until it is fullfilled
        opContinueToProcess:
        begin
          Result := AOperation(ANode, AMedianData, ARight, ARight, AMedianData);
        end;
      end;
    end;
  end;
end;

function TCustomBTree<TData>.Iterate(
  const Action: TCollIterateFunc;
  const Up: Boolean; OtherData: Pointer;
  out ABTreeNode: TBTreeNode<TData>): Pointer;
begin
  var LData: Pointer := nil;
  if InternalIterate(FRoot, Action, true, OtherData, ABTreeNode, LData) then
  begin
    Result := nil;
  end
  else
  begin
    Result := LData;
  end;
end;

procedure TCustomBTree<TData>.SetCount(const Value: Integer);
begin
  FTreeData.FCount := Value;
end;

function TBTreeNode<TData>.Find(const AData: TData; var PageIndex: Integer): TSCSearch;
begin
  if Count = 0 then
  begin
    Result := SCSPageEmpty;
    PageIndex := 0;
  end
  else if FOnCompare(AData, Data[0]) < 0 then
  begin
    Result := SCSLessThanThisPage;
    PageIndex := 0;
  end
  else if FOnCompare(AData, Data[Count-1]) > 0 then
  begin
    Result := SCSGreaterThanThisPage
  end
  else
  begin
    var L, R, M, Comp : Integer;
    {data might be in this page, check using binary search}
    Result := SCSInThisPageRange;
    L := 0;
    R := Count-1;
    repeat
      M := (L+R) div 2;
      Comp := FOnCompare(AData, Data[M]);
      if Comp > 0 then
      begin
        L := M+1
      end
      else
      begin
        R := M-1;
      if Comp = 0 then
      begin
        PageIndex := M;
        Result := SCSFound;
          if not FOnDuplicates then
          begin
            {force exit from repeat loop}
            L := M;
            {else loop to find first of a group of duplicate nodes}
          end;
        end;
      end;
    until L > R;

    if Result = SCSInThisPageRange then
    begin
    {not found in page, return where it would be inserted}
    PageIndex := M;
    if Comp > 0 then
      inc(PageIndex);
    end;
  end;
end;

procedure TCustomBTree<TData>.UpdateNextNodeIndex(const ANode: TBTreeNode<TData>; const ACount: integer);
begin
  var LNode := ANode.FNext;
  while Assigned(LNode) do
  begin
    LNode.SequentialIndex := LNode.SequentialIndex + 1;
    LNode := LNode.FNext;
  end;
end;

function TCustomBTree<TData>.InternalIterate(
  const ANode: TBTreeNode<TData>;
  const AAction: TCollIterateFunc;
  const AUp: Boolean;
  const AOtherData: Pointer;
  out ABTreeNode: TBTreeNode<TData>;
  out AData: TData): boolean;
begin
  ABTreeNode := nil;
  AData := default(TData);
  if ANode.LeafNode then
  begin
    for var idx := 0 to ANode.Count -1 do
    begin
      Result := AAction(ANode.Data^[idx], AOtherData, ABTreeNode);
      if not Result then
      begin
        ABTreeNode := ANode;
        AData := ANode.Data[idx];
        Exit;
      end;
    end;
  end
  else
  begin
    for var idx := 0 to ANode.Count -1 do
    begin
      Result := InternalIterate(ANode.Child[idx], AAction, AUp, AOtherData, ABTreeNode, AData);
      if not Result then
      begin
        Exit;
      end;
    end;
    Result := InternalIterate(ANode.Child[ANode.Count], AAction, AUp, AOtherData, ABTreeNode, AData);
    if not Result then
    begin
      Exit;
    end;
  end;
end;

function TBTreeNode<TData>.SplitNode(out ARight: TBTreeNode<TData>; out AMedianData: TData): boolean;
begin
  Result := false;
  var LMedian := (TBTreeNode<TData>.ArraySize div 2);

  AMedianData := Data[LMedian];

  if LeafNode then
  begin
    ARight := FOnCreateNode(LeafNode);
    move(Data^[LMedian], ARight.Data^[0], (LMedian+1) * SizeOf(TData));
    ARight.Count :=  LMedian+1;
    ARight.SequentialIndex := SequentialIndex + LMedian;

    fillchar(Data^[LMedian], (LMedian+1) * sizeof(TData), 0);
    Count := LMedian;
  end
  else
  begin
    ARight := FOnCreateNode(LeafNode);
    move(Data^[LMedian+1], ARight.Data^[0], (LMedian) * sizeof(TData));
    move(ChildrenPtr^[LMedian+1], ARight.ChildrenPtr^[0], (LMedian+1) * sizeof(TData));
    ARight.Count := LMedian;

    FillChar(Data^[LMedian], (LMedian+1) * sizeof(TData), 0);
    FillChar(ChildrenPtr^[LMedian+1],(LMedian+1) * sizeof(TData), 0);
    Count := LMedian;
  end;

  assert(assigned(ARight));
  if assigned(FNext) then
  begin
    FNext.FPrev := ARight;
  end;
  ARight.FPrev := Self;
  ARight.FNext := FNext;
  FNext := ARight;
  Result := true;
end;

function TCustomBTree<TData>.DoCompare(Data1, Data2 : TData) : Integer;
begin
  Result := 0;
  if Assigned(FOnCompare) then
  begin
    FOnCompare(Self, Data1, Data2, Result)
  end
  else if Assigned(FCompare) then
  begin
    Result := FCompare(Data1, Data2);
  end;
end;

end.
