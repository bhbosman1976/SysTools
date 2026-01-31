unit StGenericColl;

interface

uses
  Windows, Classes,
  StConst, StGenericBase, StGenericList, stBase;

type
  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..(StMaxBlockSize div SizeOf(Pointer))-1] of Pointer;

  TPageDescriptor = class(TStListNode<Pointer>)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;
  protected
    {PageElements count is stored in inherited Data field}
    pdPage  : PPointerArray; {Pointer to page data}
    pdStart : Integer;       {Index of first element in page}
    pdCount : Integer;       {Number of elements used in page}

  public
    constructor Create(const AData: Pointer); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;

  TStCollection = class(TStContainer<Pointer>)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;
  public
  type TCollIterateFunc = function (Container : TStCollection; Data : Pointer; OtherData : Pointer) : Boolean;


  protected
    colPageList : TStList<Pointer,TPageDescriptor>;      {List of page descriptors}
    colPageElements : Integer;  {Number of elements in a page}
    colCachePage : TPageDescriptor; {Page last found by At}

    procedure colAdjustPagesAfter(N : TPageDescriptor; Delta : Integer);
    procedure colAtInsertInPage(N : TPageDescriptor; PageIndex : Integer;
                                AData : Pointer);
    procedure colAtDeleteInPage(N : TPageDescriptor; PageIndex : Integer);
    function colGetCount : Integer;
    function colGetEfficiency : Integer;


  public
    constructor Create(PageElements : Integer); virtual;
      {-Initialize a collection with given page size and allocate first page}
    destructor Destroy; override;
      {-Free a collection}
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Clear; override;
      {-Deallocate all pages and free all items}
    procedure Pack;
      {-Squeeze collection elements into the least memory possible}

    function At(Index : Integer) : Pointer;
      {-Return the element at a given index}
    function IndexOf(Data : Pointer) : Integer; virtual;
      {-Return the index of the first item with given data}

    procedure AtInsert(Index : Integer; Data : Pointer);
      {-Insert a new element at a given index and move following items down}
    procedure AtPut(Index : Integer; Data : Pointer);
      {-Replace element at given index with new data}
    procedure Insert(Data : Pointer); virtual;
      {-Insert item at the end of the collection}

    procedure AtDelete(Index : Integer);
      {-Remove element at a given index, move following items up, free element}
    procedure Delete(Data : Pointer);
      {-Delete the first item with the given data}

    function Iterate(Action : TCollIterateFunc; Up : Boolean;
                     OtherData : Pointer) : Pointer;
      {-Call Action for all the non-nil elements, returning the last data}

    property Count : Integer
      {-Return the index of the highest assigned item, plus one}
      read colGetCount;

    property Efficiency : Integer
      {-Return the overall percent Efficiency of the pages}
      read colGetEfficiency;

    property Items[Index : Integer] : Pointer
      {-Return the Index'th node, 0-based}
      read At
      write AtPut;
      default;
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;

  {.Z+}
  TSCSearch = (SCSPageEmpty,
               SCSLessThanThisPage,
               SCSInThisPageRange,
               SCSFound,
               SCSGreaterThanThisPage);
  {.Z-}

  TStSortedCollection = class(TStCollection)
  {.Z+}
  protected
    FDuplicates : Boolean;

    function scSearchPage(AData : Pointer; N : TPageDescriptor;
                          var PageIndex : Integer) : TSCSearch;

    procedure scSetDuplicates(D : Boolean);
  {.Z-}
  public
    function DoCompare(Data1, Data2 : Pointer) : Integer;
    function IndexOf(Data : Pointer) : Integer; override;
      {-Return the index of the first item with given data}
    procedure Insert(Data : Pointer); override;
      {-Insert item in sorted position}
    property Duplicates : Boolean
      {-Determine whether sorted collection allows duplicate data}
    read FDuplicates
    write scSetDuplicates;
  end;

{======================================================================}

implementation

procedure TPageDescriptor.AfterConstruction;
begin
  inherited AfterConstruction;

  InterlockedIncrement(FCreateCount);
end;

procedure TPageDescriptor.BeforeDestruction;
begin
  InterlockedIncrement(FDestroyCount);

  inherited BeforeDestruction;
end;

constructor TPageDescriptor.Create(const AData: Pointer);
begin
  inherited Create(AData);
  GetMem(pdPage, Integer(Data)*SizeOf(Pointer));
  FillChar(pdPage^, Integer(Data)*SizeOf(Pointer), 0);
end;

destructor TPageDescriptor.Destroy;
begin
  if Assigned(pdPage) then
    FreeMem(pdPage, Integer(Data)*SizeOf(Pointer));
  inherited Destroy;
end;

procedure TStCollection.AfterConstruction;
begin
  inherited AfterConstruction;

  InterlockedIncrement(FCreateCount);
end;

{----------------------------------------------------------------------}


function TStCollection.At(Index : Integer) : Pointer;
var
  Start : Integer;
  N : TPageDescriptor;
begin

  EnterCS;
  try

    if Index < 0 then
      RaiseContainerError(stscBadIndex);

    N := colCachePage;
    if Index >= N.pdStart then
      {search up}
      repeat
        with N do begin
          Start := pdStart;
          if Index < Start then begin
            {element has not been set}
            colCachePage := N;
            break;
          end else if Index < Start+pdCount then begin
            {element is in this page}
            colCachePage := N;
            Result := pdPage^[Index-Start];
            Exit;
          end;
        end;
        N := TPageDescriptor(N.FNext);
      until not Assigned(N)

    else begin
      {search down}
      N := TPageDescriptor(N.FPrev);
      while Assigned(N) do begin
        with N do begin
          Start := pdStart;
          if (Index >= Start+pdCount) then begin
            {element has not been set}
            colCachePage := N;
            break;
          end else if Index >= Start then begin
            {element is in this page}
            colCachePage := N;
            Result := pdPage^[Index-Start];
            Exit;
          end;
        end;
        N := TPageDescriptor(N.FPrev);
      end;
    end;

    {not found, leave cache page unchanged}
    Result := nil;

  finally
    LeaveCS;
  end;

end;

procedure TStCollection.AtDelete(Index : Integer);
var
  Start : Integer;
  N : TPageDescriptor;
begin

  EnterCS;
  try

    if Index < 0 then
      RaiseContainerError(stscBadIndex);

    N := colCachePage;
    if Index >= N.pdStart then
      repeat
        with N do begin
          Start := pdStart;
          if Index < Start then begin
            {element has not been set, nothing to free}
            Dec(pdStart);
            colAdjustPagesAfter(N, -1);
            colCachePage := N;
            Exit;
          end else if Index < Start+pdCount then begin
            {element is in this page}
            colCachePage := N;
            colAtDeleteInPage(N, Index-Start);
            Exit;
          end;
        end;
        N := TPageDescriptor(N.FNext);
      until not Assigned(N)

    else begin
      {search down}
      N := TPageDescriptor(N.FPrev);
      while Assigned(N) do begin
        with N do begin
          Start := pdStart;
          if Index >= Start+pdCount then begin
            {element has not been set, nothing to free}
            Dec(pdStart);
            colAdjustPagesAfter(N, -1);
            colCachePage := N;
            Exit;
          end else if Index >= Start then begin
            {element is in this page}
            colCachePage := N;
            colAtDeleteInPage(N, Index-Start);
            Exit;
          end;
        end;
        N := TPageDescriptor(N.FPrev);
      end;
    end;

    {index not found, nothing to delete}

  finally
    LeaveCS;
  end;

end;

procedure TStCollection.AtInsert(Index : Integer; Data : Pointer);
var
  Start : Integer;
  NC : Integer;
  N : TPageDescriptor;
begin

  EnterCS;
  try

  if Index < 0 then
      RaiseContainerError(stscBadIndex);

    N := TPageDescriptor(colPageList.Head);
    while Assigned(N) do begin
      Start := N.pdStart;
      if Index < Start then begin
        {current page has indexes greater than the specified one}
        if Start-Index <= colPageElements-N.pdCount then begin
          {room to squeeze element into this page}
          NC := Start-Index;
          Move(N.pdPage^[0], N.pdPage^[NC], N.pdCount*SizeOf(Pointer));
          FillChar(N.pdPage^[1], (NC-1)*SizeOf(Pointer), 0);
          Inc(N.pdCount, NC);
        end else begin
          {insert on a new page before this one}
          N := TPageDescriptor(colPageList.PlaceBefore(Pointer(colPageElements), N));
          N.pdCount := 1;
        end;
        N.pdStart := Index;
        N.pdPage^[0] := Data;
        colAdjustPagesAfter(N, +1);
        Exit;
      end else if Index < Start+colPageElements then
        if (not Assigned(N.FNext)) or (Index < TPageDescriptor(N.FNext).pdStart) then begin
          {should be inserted on this page}
          colAtInsertInPage(N, Index-Start, Data);
          Exit;
        end;
      N := TPageDescriptor(N.FNext);
    end;

    {should be inserted after all existing pages}
    N := TPageDescriptor(colPageList.Append(Pointer(colPageElements)));
    N.pdStart := Index;
    N.pdCount := 1;
    N.pdPage^[0] := Data;

  finally
    LeaveCS;
  end;

end;

procedure TStCollection.AtPut(Index : Integer; Data : Pointer);
var
  Start : Integer;
  N, T : TPageDescriptor;
begin

  EnterCS;
  try

    if Index < 0 then
      RaiseContainerError(stscBadIndex);

    {special case for putting to end of collection}
    T := TPageDescriptor(colPageList.Tail);
    if Index = T.pdStart+T.pdCount then begin
      if T.pdCount >= colPageElements then begin
        {last page is full, add another}
        Start := T.pdStart+colPageElements;
        T := TPageDescriptor(colPageList.Append(Pointer(colPageElements)));
        T.pdStart := Start;
        {T.pdCount := 0;}
      end;
      T.pdPage^[T.pdCount] := Data;
      inc(T.pdCount);
      Exit;
    end;

    N := colCachePage;
    if Index >= N.pdStart then
      {search up}
      repeat
        Start := N.pdStart;
        if Index < Start then begin
          {element has not been set before}
          N := TPageDescriptor(colPageList.PlaceBefore(Pointer(colPageElements), N));
          N.pdStart := Index;
          N.pdCount := 1;
          N.pdPage^[0] := Data;
          colCachePage := N;
          Exit;
        end else if Index < Start+N.pdCount then begin
          {element fits in this page}
          colCachePage := N;
          N.pdPage^[Index-Start] := Data;
          Exit;
        end else if (N = T) and (Index < Start+colPageElements) then begin
          {element fits in last page}
          colCachePage := N;
          N.pdPage^[Index-Start] := Data;
          N.pdCount := Index-Start+1;
          Exit;
        end;
        N := TPageDescriptor(N.FNext);
      until not Assigned(N)

    else begin
      {search down}
      N := TPageDescriptor(N.FPrev);
      while Assigned(N) do begin
        Start := N.pdStart;
        if (Index >= Start+N.pdCount) then begin
          {element has not been set before}
          N := TPageDescriptor(colPageList.PlaceBefore(Pointer(colPageElements), N));
          N.pdStart := Index;
          N.pdCount := 1;
          N.pdPage^[0] := Data;
          colCachePage := N;
          Exit;
        end else if Index >= Start then begin
          {element is in this page}
          colCachePage := N;
          N.pdPage^[Index-Start] := Data;
          Exit;
        end;
        N := TPageDescriptor(N.FPrev);
      end;
    end;

    {an element after all existing ones}
    N := TPageDescriptor(colPageList.Append(Pointer(colPageElements)));
    colCachePage := N;
    N.pdStart := Index;
    N.pdCount := 1;
    N.pdPage^[0] := Data;
    Exit;

  finally
    LeaveCS;
  end;

end;

procedure TStCollection.BeforeDestruction;
begin
  InterlockedIncrement(FDestroyCount);

  inherited BeforeDestruction;
end;

procedure TStCollection.Clear;
var
  I : Integer;
  N, P : TPageDescriptor;
begin

  EnterCS;
  try

    N := TPageDescriptor(colPageList.Head);
    colCachePage := N;
    while Assigned(N) do begin
      for I := 0 to N.pdCount-1 do
        DoDisposeData(N.pdPage^[I]);
      P := TPageDescriptor(N.FNext);
      if N = colCachePage then begin
        {keep the first page, which is now empty}
        N.pdCount := 0;
        N.pdStart := 0;
      end else
        {delete all other pages}
        colPageList.Delete(N);
      N := P;
    end;

  finally
    LeaveCS;
  end;

end;

procedure TStCollection.colAdjustPagesAfter(N : TPageDescriptor; Delta : Integer);
begin
  N := TPageDescriptor(N.FNext);
  while Assigned(N) do begin
    inc(N.pdStart, Delta);
    N := TPageDescriptor(N.FNext);
  end;
end;

procedure TStCollection.colAtDeleteInPage(N : TPageDescriptor; PageIndex : Integer);
begin
  with N do begin
    {free the element}
    DoDisposeData(pdPage^[PageIndex]);
    Move(pdPage^[PageIndex+1], pdPage^[PageIndex],
         (colPageElements-PageIndex-1)*SizeOf(Pointer));
    Dec(pdCount);
    colAdjustPagesAfter(N, -1);
    if (pdCount = 0) and (colPageList.Count > 1) then begin
      {delete page if at least one page will remain}
      if N = colCachePage then begin                           
        colCachePage := TPageDescriptor(colPageList.Head);
        if N = colCachePage then                               
          colCachePage := TPageDescriptor(N.FNext);            
      end;
      colPageList.Delete(N);
    end;
  end;
end;

procedure TStCollection.colAtInsertInPage(N : TPageDescriptor; PageIndex : Integer;
                                       AData : Pointer);
var
  P : TPageDescriptor;
  PC : Integer;
begin
  with N do
    if pdCount >= colPageElements then begin
      {page is full, add another}
      P := TPageDescriptor(colPageList.Place(Pointer(colPageElements), N));
      {new page starts with element after the new one}
      P.pdStart := pdStart+PageIndex+1;
      PC := colPageElements-PageIndex;
      Move(pdPage^[PageIndex], P.pdPage^[0], PC*SizeOf(Pointer));
      pdPage^[PageIndex] := AData;
      pdCount := PageIndex+1;
      P.pdCount := PC;
      colAdjustPagesAfter(P, +1);
    end else begin
      {room to add on this page}
      if pdCount > PageIndex then begin
        Move(pdPage^[PageIndex], pdPage^[PageIndex+1], (pdCount-PageIndex)*SizeOf(Pointer));
        colAdjustPagesAfter(N, +1);
        inc(pdCount);
      end else begin
        FillChar(pdPage^[pdCount], (PageIndex-pdCount)*SizeOf(Pointer), 0);
        colAdjustPagesAfter(N, PageIndex+1-pdCount);
        pdCount := PageIndex+1;
      end;
      pdPage^[PageIndex] := AData;
    end;
end;

function TStCollection.colGetCount : Integer;
begin

  EnterCS;
  try

    with TPageDescriptor(colPageList.Tail) do
      Result := pdStart+pdCount;

  finally
    LeaveCS;
  end;

end;

function TStCollection.colGetEfficiency : Integer;
var
  Pages, ECount : Integer;
  N : TPageDescriptor;
begin

  EnterCS;
  try

    ECount := 0;
    Pages := 0;
    N := TPageDescriptor(colPageList.Head);
    while Assigned(N) do begin
      with N do begin
        inc(Pages);
        inc(ECount, N.pdCount);
      end;
      N := TPageDescriptor(N.FNext);
    end;
    Result := (100*ECount) div (Pages*colPageElements);

  finally
    LeaveCS;
  end;

end;


constructor TStCollection.Create(PageElements : Integer);
begin
  inherited Create;

  if (PageElements = 0) then
    RaiseContainerError(stscBadSize);

  colPageList := TStList<Pointer,TPageDescriptor>.Create();
  colPageElements := PageElements;

  {start with one empty page}
  colPageList.Append(Pointer(colPageElements));
  colCachePage := TPageDescriptor(colPageList.Head);
end;

procedure TStCollection.Delete(Data : Pointer);
var
  Index : Integer;
begin

  EnterCS;
  try

    Index := IndexOf(Data);
    if Index >= 0 then
      AtDelete(Index);

  finally
    LeaveCS;
  end;

end;

destructor TStCollection.Destroy;
begin
  Clear;
  colPageList.Free;
  IncNodeProtection;
  inherited Destroy;
end;

function TStCollection.IndexOf(Data : Pointer) : Integer;
var
  I : Integer;
  N : TPageDescriptor;
begin

  EnterCS;
  try

    N := TPageDescriptor(colPageList.Head);
    while Assigned(N) do begin
      for I := 0 to N.pdCount-1 do
        if N.pdPage^[I] = Data then begin
          colCachePage := N;
          Result := N.pdStart+I;
          Exit;
        end;
      N := TPageDescriptor(N.FNext);
    end;
    IndexOf := -1;

  finally
    LeaveCS;
  end;

end;

procedure TStCollection.Insert(Data : Pointer);
var
  Start : Integer;
  N : TPageDescriptor;
begin

  EnterCS;
  try

    N := TPageDescriptor(colPageList.Tail);
    if N.pdCount >= colPageElements then begin
      {last page is full, add another}
      Start := N.pdStart+colPageElements;
      N := TPageDescriptor(colPageList.Append(Pointer(colPageElements)));
      N.pdStart := Start;
      {N.pdCount := 0;}
    end;
    N.pdPage^[N.pdCount] := Data;
    inc(N.pdCount);

  finally
    LeaveCS;
  end;

end;

function TStCollection.Iterate(Action : TCollIterateFunc; Up : Boolean;
                               OtherData : Pointer) : Pointer;
var
  I : Integer;
  N : TPageDescriptor;
begin

  EnterCS;
  try

    if Up then begin
      N := TPageDescriptor(colPageList.Head);
      while Assigned(N) do begin
        with N do
          for I := 0 to pdCount-1 do
            if (pdPage^[I] <> nil) then
              if not Action(Self, pdPage^[I], OtherData) then begin
                Result := pdPage^[I];
                Exit;
              end;
        N := TPageDescriptor(N.FNext);
      end;
    end else begin
      N := TPageDescriptor(colPageList.Tail);
      while Assigned(N) do begin
        with N do
          for I := pdCount-1 downto 0 do
            if (pdPage^[I] <> nil) then
              if not Action(Self, pdPage^[I], OtherData) then begin
                Result := pdPage^[I];
                Exit;
              end;
        N := TPageDescriptor(N.FPrev);
      end;
    end;

    Result := nil;

  finally
    LeaveCS;
  end;

end;

procedure TStCollection.Pack;
var
  N, P : TPageDescriptor;
  NC : Integer;
begin

  EnterCS;
  try

    colCachePage := TPageDescriptor(colPageList.Head);
    N := colCachePage;
    while Assigned(N) do begin
      while Assigned(N.FNext) and (N.pdCount < colPageElements) do begin
        {there is a page beyond this page and room to add to this page}
        P := TPageDescriptor(N.FNext);
        if N.pdStart+N.pdCount = P.pdStart then begin
          {next page has contiguous elements}
          NC := colPageElements-N.pdCount;
          if NC > P.pdCount then
            NC := P.pdCount;
          move(P.pdPage^[0], N.pdPage^[N.pdCount], NC*SizeOf(Pointer));
          move(P.pdPage^[NC], P.pdPage^[0], (P.pdCount-NC)*SizeOf(Pointer));
          inc(N.pdCount, NC);
          dec(P.pdCount, NC);
          if P.pdCount = 0 then
            colPageList.Delete(P)
          else
            inc(P.pdStart, NC);
        end else
          {pages aren't contiguous, can't merge}
          break;
      end;
      N := TPageDescriptor(N.FNext);
    end;

  finally
    LeaveCS;
  end;

end;


{----------------------------------------------------------------------}

function TStSortedCollection.IndexOf(Data : Pointer) : Integer;
var
  N : TPageDescriptor;
  PageIndex : Integer;
begin

  EnterCS;
  try

    if (Count = 0) then begin
      Result := -1;
      Exit;
    end;
    N := colCachePage;
    if DoCompare(Data, N.pdPage^[0]) >= 0 then begin
      {search up}
      repeat
        case scSearchPage(Data, N, PageIndex) of
          SCSFound :
            begin
              colCachePage := N;
              Result := N.pdStart+PageIndex;
              Exit;
            end;
          SCSGreaterThanThisPage :
            {keep on searching} ;
        else
          {can't be anywhere else in the collection}
          break;
        end;
        N := TPageDescriptor(N.FNext);
      until not Assigned(N);

    end else begin
      {search down}
      N := TPageDescriptor(N.FPrev);
      while Assigned(N) do begin
        case scSearchPage(Data, N, PageIndex) of
          SCSFound :
            begin
              colCachePage := N;
              Result := N.pdStart+PageIndex;
              Exit;
            end;
          SCSLessThanThisPage :
            {keep on searching} ;
        else
          {can't be anywhere else in the collection}
          break;
        end;
        N := TPageDescriptor(N.FPrev);
      end;
    end;

    Result := -1;

  finally
    LeaveCS;
  end;

end;

procedure TStSortedCollection.Insert(Data : Pointer);
var
  N : TPageDescriptor;
  PageIndex : Integer;
begin

  EnterCS;
  try

    N := TPageDescriptor(colPageList.Head);
    while Assigned(N) do begin
      case scSearchPage(Data, N, PageIndex) of
        SCSPageEmpty, SCSInThisPageRange, SCSLessThanThisPage :
          begin
            colAtInsertInPage(N, PageIndex, Data);
            Exit;
          end;
        SCSFound :
          if FDuplicates then begin
            colAtInsertInPage(N, PageIndex, Data);
            Exit;
          end else
          RaiseContainerError(stscDupNode);
      end;
      N := TPageDescriptor(N.FNext);
    end;

    {greater than all other items}
    inherited Insert(Data);

  finally
    LeaveCS;
  end;

end;

function TStSortedCollection.scSearchPage(AData : Pointer; N : TPageDescriptor;
                                          var PageIndex : Integer) : TSCSearch;
var
  L, R, M, Comp : Integer;
begin
  with N do
    if pdCount = 0 then begin
      Result := SCSPageEmpty;
      PageIndex := 0;
    end else if DoCompare(AData, pdPage^[0]) < 0 then begin
      Result := SCSLessThanThisPage;
      PageIndex := 0;
    end else if DoCompare(AData, pdPage^[pdCount-1]) > 0 then
      Result := SCSGreaterThanThisPage
    else begin
      {data might be in this page, check using binary search}
      Result := SCSInThisPageRange;
      L := 0;
      R := pdCount-1;
      repeat
        M := (L+R) div 2;
        Comp := DoCompare(AData, pdPage^[M]);
        if Comp > 0 then
          L := M+1
        else begin
          R := M-1;
        if Comp = 0 then begin
          PageIndex := M;
          Result := SCSFound;
            if not FDuplicates then
              {force exit from repeat loop}
              L := M;
            {else loop to find first of a group of duplicate nodes}
          end;
        end;
      until L > R;

      if Result = SCSInThisPageRange then begin
      {not found in page, return where it would be inserted}
      PageIndex := M;
      if Comp > 0 then
        inc(PageIndex);
    end;
end;
end;

procedure TStSortedCollection.scSetDuplicates(D : Boolean);
begin
  if FDuplicates <> D then
    if D then
      FDuplicates := True
    else if FCount <> 0 then
      RaiseContainerError(stscBadDups)
    else
      FDuplicates := False;
end;


function TStSortedCollection.DoCompare(Data1, Data2 : Pointer) : Integer;
begin
  Result := 0;
  if Assigned(FOnCompare) then
    FOnCompare(Self, Data1, Data2, Result)
  else if Assigned(FCompare) then
    Result := FCompare(Data1, Data2);
end;




end.
