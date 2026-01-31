unit StGenericDQue;

interface

uses
  Windows,
  STConst, StGenericBase, StGenericList;

type
  TStDQue<TData,TNode:TStListNode<TData>> = class(TStList<TData, TNode>)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;

    public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
      procedure PushTail(Data : TData);
        {-Add element at tail of queue}
      procedure PopTail;
        {-Delete element at tail of queue, destroys its data}
      procedure PeekTail(var Data : TData);
        {-Return data at tail of queue}

      procedure PushHead(Data : TData);
        {-Add element at head of queue}
      procedure PopHead;
        {-Delete element at head of queue, destroys its data}
      procedure PeekHead(var Data : TData);
        {-Return data at head of queue}
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;

{======================================================================}

implementation



procedure TStDQue<TData, TNode>.AfterConstruction;
begin
  inherited AfterConstruction;

  InterlockedIncrement(FCreateCount);
end;

procedure TStDQue<TData, TNode>.BeforeDestruction;
begin
  InterlockedIncrement(FDestroyCount);

  inherited BeforeDestruction;
end;

procedure TStDQue<TData,TNode>.PeekHead(var Data : TData);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count = 0 then
      Data := nil
    else
      Data := Head.Data;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStDQue<TData,TNode>.PeekTail(var Data : TData);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count = 0 then
      Data := nil
    else
      Data := Tail.Data;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStDQue<TData,TNode>.PopHead;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count > 0 then
      Delete(Head);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStDQue<TData,TNode>.PopTail;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count > 0 then
      Delete(Tail);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStDQue<TData,TNode>.PushHead(Data : TData);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Insert(Data);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStDQue<TData,TNode>.PushTail(Data : TData);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Append(Data);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;


end.
