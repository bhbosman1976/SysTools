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
{* SysTools: StTree.pas 4.04                             *}
{*********************************************************}
{* SysTools: AVL Tree class                              *}
{*********************************************************}

{$I StDefine.inc}

{Notes:
  - These binary trees are self-balancing in the AVL sense (the depth
    of any left branch differs by no more than one from the depth of the
    right branch).

  - Duplicate data is not allowed in a tree.

  - Nodes can be of type TStTreeNode or any descendant.

  - The Compare property of the TStContainer ancestor must be set to
    specify the sort order of the tree. The Compare function operates
    on Data pointers. The Data pointer could be typecast to a number
    (any integer type), to a string pointer, to a record pointer, or to
    an instance of a class.

  - Next and Prev should not be used to iterate through an entire tree.
    This is much slower than calling the Iterate method.
}

unit StGenericTree;

interface

uses
  Windows,
  SysUtils, Classes, StConst, StGenericBase;

type
  TStTreeNode<TData> = class(TStNode<TData>)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;
    protected
      tnPos  : array[Boolean] of TStTreeNode<TData>; {Child nodes}
      tnBal  : Integer;         {Used during balancing}

  public
    constructor Create(); override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;
  TStTree<TData> = class(TStContainer<TData>)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;
  public
    type TIterateFunc<TOtherData> = function(Container : TStTree<TData>; Node : TStTreeNode<TData>; OtherData : TOtherData) : Boolean of object;
    type
      SplitRec =
      record
        SData : Pointer;
        STree : TStTree<TData>;
      end;

    const
      StackSize = 40;

    type
      StackNode =
        record
          Node : TStTreeNode<TData>;
          Comparison : Integer;
        end;
      StackArray = array[1..StackSize] of StackNode;


    class function DestroyNode(Container : TStTree<TData>; Node : TStTreeNode<TData>; OtherData : Pointer) : Boolean;
    class function JoinNode(Container : TStTree<TData>; Node : TStTreeNode<TData>; OtherData : TStTree<TData>) : Boolean;
    class procedure DelBalance(var P : TStTreeNode<TData>; var SubTreeDec : Boolean; CmpRes : Integer);
    class procedure InsBalance(var P : TStTreeNode<TData>; var SubTreeInc : Boolean; CmpRes : Integer);
    class function Sign(I : Integer) : Integer;
    class var ClassCritSect : TRTLCriticalSection;
    class procedure EnterClassCS;
    class procedure LeaveClassCS;

  protected
    trRoot : TStTreeNode<TData>;       {Root of tree}
    trIgnoreDups : Boolean;     {Ignore duplicates during Join?}
    procedure trInsertNode(N : TStTreeNode<TData>);
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create(); virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function DoCompare(Data1, Data2 : TData) : Integer;
    procedure Clear; override;
    function Insert(Data : TData) : TStTreeNode<TData>;
    procedure Delete(Data : TData);
    function Find(Data : TData) : TStTreeNode<TData>;
    procedure Join(T: TStTree<TData>; IgnoreDups : Boolean);
    function Iterate<TOtherData>(Action : TIterateFunc<TOtherData>; Up : Boolean; OtherData : TOtherData) : TStTreeNode<TData>;
    function First : TStTreeNode<TData>;
    function Last : TStTreeNode<TData>;
    function Next(N : TStTreeNode<TData>) : TStTreeNode<TData>;
    function Prev(N : TStTreeNode<TData>) : TStTreeNode<TData>;
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;

{======================================================================}

implementation


class destructor TStTree<TData>.Destroy;
begin

  Windows.DeleteCriticalSection(ClassCritSect);

end;

class function TStTree<TData>.DestroyNode(Container : TStTree<TData>; Node : TStTreeNode<TData>; OtherData : Pointer) : Boolean;
begin
  Container.DisposeNodeData(Node);
  Node.Free;
  Result := True;
end;

class procedure TStTree<TData>.EnterClassCS;
begin

  EnterCriticalSection(ClassCritSect);

end;

class procedure TStTree<TData>.LeaveClassCS;
begin

  LeaveCriticalSection(ClassCritSect);

end;

const
  Left = False;
  Right = True;

{Following stack declarations are used to avoid recursion in all tree
 routines. Because the tree is AVL-balanced, a stack size of 40
 allows at least 2**32 elements in the tree without overflowing the
 stack.}


procedure TStTreeNode<TData>.AfterConstruction;
begin
  inherited AfterConstruction;

  InterlockedIncrement(FCreateCount);
end;

procedure TStTreeNode<TData>.BeforeDestruction;
begin
  InterlockedIncrement(FDestroyCount);

  inherited BeforeDestruction;
end;

constructor TStTreeNode<TData>.Create;
begin
  inherited Create;
end;



function TStTree<TData>.DoCompare(Data1, Data2 : TData) : Integer;
begin
  Result := 0;
  if Assigned(FOnCompare) then
    FOnCompare(Self, Data1, Data2, Result)
  else if Assigned(FCompare) then
    Result := FCompare(Data1, Data2);
end;


{----------------------------------------------------------------------}

class function TStTree<TData>.Sign(I : Integer) : Integer;
begin
  if I < 0 then
    Sign := -1
  else if I > 0 then
    Sign := +1
  else
    Sign := 0;
end;

class procedure TStTree<TData>.DelBalance(var P : TStTreeNode<TData>; var SubTreeDec : Boolean; CmpRes : Integer);
var
  P1, P2 : TStTreeNode<TData>;
  B1, B2 : Integer;
  LR : Boolean;
begin
  CmpRes := Sign(CmpRes);
  if P.tnBal = CmpRes then
    P.tnBal := 0
  else if P.tnBal = 0 then begin
    P.tnBal := -CmpRes;
    SubTreeDec := False;
  end else begin
    LR := (CmpRes < 0);
    P1 := P.tnPos[LR];
    B1 := P1.tnBal;
    if (B1 = 0) or (B1 = -CmpRes) then begin
      {Single RR or LL rotation}
      P.tnPos[LR] := P1.tnPos[not LR];
      P1.tnPos[not LR] := P;
      if B1 = 0 then begin
        P.tnBal := -CmpRes;
        P1.tnBal := CmpRes;
        SubTreeDec := False;
      end else begin
        P.tnBal := 0;
        P1.tnBal := 0;
      end;
      P := P1;
    end else begin
      {Double RL or LR rotation}
      P2 := P1.tnPos[not LR];
      B2 := P2.tnBal;
      P1.tnPos[not LR] := P2.tnPos[LR];
      P2.tnPos[LR] := P1;
      P.tnPos[LR] := P2.tnPos[not LR];
      P2.tnPos[not LR] := P;
      if B2 = -CmpRes then
        P.tnBal := CmpRes
      else
        P.tnBal := 0;
      if B2 = CmpRes then
        P1.tnBal := -CmpRes
      else
        P1.tnBal := 0;
      P := P2;
      P2.tnBal := 0;
    end;
  end;
end;

class procedure TStTree<TData>.InsBalance(var P : TStTreeNode<TData>; var SubTreeInc : Boolean; CmpRes : Integer);
var
  P1 : TStTreeNode<TData>;
  P2 : TStTreeNode<TData>;
  LR : Boolean;
begin
  CmpRes := Sign(CmpRes);
  if P.tnBal = -CmpRes then begin
    P.tnBal := 0;
    SubTreeInc := False;
  end else if P.tnBal = 0 then
    P.tnBal := CmpRes
  else begin
    LR := (CmpRes > 0);
    P1 := P.tnPos[LR];
    if P1.tnBal = CmpRes then begin
      P.tnPos[LR] := P1.tnPos[not LR];
      P1.tnPos[not LR] := P;
      P.tnBal := 0;
      P := P1;
    end else begin
      P2 := P1.tnPos[not LR];
      P1.tnPos[not LR] := P2.tnPos[LR];
      P2.tnPos[LR] := P1;
      P.tnPos[LR] := P2.tnPos[not LR];
      P2.tnPos[not LR] := P;
      if P2.tnBal = CmpRes then
        P.tnBal := -CmpRes
      else
        P.tnBal := 0;
      if P2.tnBal = -CmpRes then
        P1.tnBal := CmpRes
      else
        P1.tnBal := 0;
      P := P2;
    end;
    P.tnBal := 0;
    SubTreeInc := False;
  end;
end;

class function TStTree<TData>.JoinNode(Container : TStTree<TData>; Node : TStTreeNode<TData>; OtherData : TStTree<TData>) : Boolean;
var
  N : TStTreeNode<TData>;
begin
  Result := True;
  N := TStTree<TData>(OtherData).Find(Node.Data);
  if Assigned(N) then
    if TStTree<TData>(OtherData).trIgnoreDups then begin
      Node.Free;
      Exit;
    end else
      RaiseContainerError(stscDupNode);

  with TStTreeNode<TData>(Node) do begin
    tnPos[Left] := nil;
    tnPos[Right] := nil;
    tnBal := 0;
  end;
  TStTree<TData>(OtherData).trInsertNode(TStTreeNode<TData>(Node));
end;



procedure TStTree<TData>.AfterConstruction;
begin
  inherited AfterConstruction;

  InterlockedIncrement(FCreateCount);
end;

procedure TStTree<TData>.BeforeDestruction;
begin
  InterlockedIncrement(FDestroyCount);

  inherited BeforeDestruction;
end;

{----------------------------------------------------------------------}

procedure TStTree<TData>.Clear;
begin

  EnterCS;
  try

    if conNodeProt = 0 then
      Iterate<Pointer>(DestroyNode, True, nil);
    trRoot := nil;
    FCount := 0;

  finally
    LeaveCS;
  end;

end;



constructor TStTree<TData>.Create();
begin
  inherited Create;
end;

class constructor TStTree<TData>.Create;
begin

  Windows.InitializeCriticalSection(ClassCritSect);

end;

procedure TStTree<TData>.Delete(Data : TData);
var
  P : TStTreeNode<TData>;
  Q : TStTreeNode<TData>;
  TmpData : TData;
  CmpRes : Integer;
  Found : Boolean;
  SubTreeDec : Boolean;
  StackP : Integer;
  Stack : StackArray;
begin

  EnterCS;
  try

    P := trRoot;
    if not Assigned(P) then
      Exit;

    {Find node to delete and stack the nodes to reach it}
    Found := False;
    StackP := 0;
    while not Found do begin
      CmpRes := DoCompare(Data, P.Data);
      Inc(StackP);
      if CmpRes = 0 then begin
        {Found node to delete}
        with Stack[StackP] do begin
          Node := P;
          Comparison := -1;
        end;
        Found := True;
      end else begin
        with Stack[StackP] do begin
          Node := P;
          Comparison := CmpRes;
        end;
        P := P.tnPos[CmpRes > 0];
        if not Assigned(P) then
          {Node to delete not found}
          Exit;
      end;
    end;

    {Delete the node found}
    Q := P;
    if (not Assigned(Q.tnPos[Right])) or (not Assigned(Q.tnPos[Left])) then begin
      {Node has at most one branch}
      Dec(StackP);
      P := Q.tnPos[Assigned(Q.tnPos[Right])];
      if StackP = 0 then
        trRoot := P
      else with Stack[StackP] do
        Node.tnPos[Comparison > 0] := P;
    end else begin
      {Node has two branches; stack nodes to reach one with no right child}
      P := Q.tnPos[Left];
      while Assigned(P.tnPos[Right]) do begin
        Inc(StackP);
        with Stack[StackP] do begin
          Node := P;
          Comparison := 1;
        end;
        P := P.tnPos[Right];
      end;

      {Swap the node to delete with the terminal node}
      TmpData := Q.Data;
      Q.Data := P.Data;
      Q := P;
      with Stack[StackP] do begin
        Node.tnPos[Comparison > 0].Data := TmpData;
        Node.tnPos[Comparison > 0] := P.tnPos[Left];
      end;
    end;

    {Dispose of the deleted node}
    DisposeNodeData(Q);
    Q.Free;
    Dec(FCount);

    {Unwind the stack and rebalance}
    SubTreeDec := True;
    while (StackP > 0) and SubTreeDec do begin
      if StackP = 1 then
        DelBalance(trRoot, SubTreeDec, Stack[1].Comparison)
      else with Stack[StackP-1] do
        DelBalance(Node.tnPos[Comparison > 0], SubTreeDec, Stack[StackP].Comparison);
      dec(StackP);
    end;

  finally
    LeaveCS;
  end;

end;

function TStTree<TData>.Find(Data : TData) : TStTreeNode<TData>;
var
  P : TStTreeNode<TData>;
  CmpRes : Integer;
begin

  EnterCS;
  try

    P := trRoot;
    while Assigned(P) do begin
      CmpRes := DoCompare(Data, P.Data);
      if CmpRes = 0 then begin
        Result := P;
        Exit;
      end else
        P := P.tnPos[CmpRes > 0];
    end;

    Result := nil;

  finally
    LeaveCS;
  end;

end;

function TStTree<TData>.First : TStTreeNode<TData>;
begin

  EnterCS;
  try

    if Count = 0 then
      Result := nil
    else begin
      Result := trRoot;
      while Assigned(Result.tnPos[Left]) do
        Result := Result.tnPos[Left];
    end;

  finally
    LeaveCS;
  end;

end;

function TStTree<TData>.Insert(Data : TData) : TStTreeNode<TData>;
begin

  EnterCS;
  try

    {Create the node}
    Result :=  TStTreeNode<TData>.create;
    Result.Init(Data);
    trInsertNode(Result);

  finally
    LeaveCS;
  end;

end;

function TStTree<TData>.Iterate<TOtherData>(Action : TIterateFunc<TOtherData>; Up : Boolean;
                         OtherData : TOtherData) : TStTreeNode<TData>;
var
  P : TStTreeNode<TData>;
  Q : TStTreeNode<TData>;
  StackP : Integer;
  Stack : StackArray;
begin
  EnterCS;
  try
    StackP := 0;
    P := trRoot;
    repeat
      while Assigned(P) do begin
        Inc(StackP);
        Stack[StackP].Node := P;
        P := P.tnPos[not Up];
      end;
      if StackP = 0 then begin
        Result := nil;
        Exit;
      end;

      P := Stack[StackP].Node;
      Dec(StackP);
      Q := P;
      P := P.tnPos[Up];
      if not Action(Self, Q, OtherData) then begin
        Result := Q;
        Exit;
      end;
    until False;
  finally
    LeaveCS;
  end;
end;

procedure TStTree<TData>.Join(T: TStTree<TData>; IgnoreDups : Boolean);
begin

  EnterClassCS;
  EnterCS;
  T.EnterCS;
  try
    trIgnoreDups := IgnoreDups;
    T.Iterate<TStTree<TData>>(JoinNode, True, Self);
    T.IncNodeProtection;
    T.Free;

  finally
    T.LeaveCS;
    LeaveCS;
    LeaveClassCS;
  end;
end;

function TStTree<TData>.Last : TStTreeNode<TData>;
begin

  EnterCS;
  try

    if Count = 0 then
      Result := nil
    else begin
      Result := trRoot;
      while Assigned(Result.tnPos[Right]) do
        Result := Result.tnPos[Right];
    end;

  finally
    LeaveCS;
  end;

end;

function TStTree<TData>.Next(N : TStTreeNode<TData>) : TStTreeNode<TData>;
var
  Found : Word;
  P : TStTreeNode<TData>;
  StackP : Integer;
  Stack : StackArray;
begin
  EnterCS;
  try
    Result := nil;
    Found := 0;
    StackP := 0;
    P := trRoot;
    repeat
      while Assigned(P) do begin
        Inc(StackP);
        Stack[StackP].Node := P;
        P := P.tnPos[Left];
      end;
      if StackP = 0 then
        Exit;

      P := Stack[StackP].Node;
      Dec(StackP);
      if Found = 1 then begin
        Result := P;
        Exit;
      end;
      if P = N then
        Inc(Found);
      P := P.tnPos[Right];
    until False;
  finally
    LeaveCS;
  end;
end;

function TStTree<TData>.Prev(N : TStTreeNode<TData>) : TStTreeNode<TData>;
var
  Found : Word;
  P : TStTreeNode<TData>;
  StackP : Integer;
  Stack : StackArray;
begin
  EnterCS;
  try
    Result := nil;
    Found := 0;
    StackP := 0;
    P := trRoot;
    repeat
      while Assigned(P) do begin
        Inc(StackP);
        Stack[StackP].Node := P;
        P := P.tnPos[Right];
      end;
      if StackP = 0 then
        Exit;

      P := Stack[StackP].Node;
      Dec(StackP);
      if Found = 1 then begin
        Result := P;
        Exit;
      end;
      if P = N then
        Inc(Found);
      P := P.tnPos[Left];
    until False;
  finally
    LeaveCS;
  end;
end;


procedure TStTree<TData>.trInsertNode(N : TStTreeNode<TData>);
var
  P : TStTreeNode<TData>;
  CmpRes : Integer;
  StackP : Integer;
  Stack : StackArray;
  SubTreeInc : Boolean;
begin
  if not Assigned(N) then
    Exit;

  {Handle first node}
  P := trRoot;
  if not Assigned(P) then begin
    trRoot := N;
    Inc(FCount);
    Exit;
  end;

  {Find where new node should fit in tree}
  StackP := 0;
  CmpRes := 0; {prevent D32 from generating a warning}
  while Assigned(P) do begin
    CmpRes := DoCompare(N.Data, P.Data);
    if CmpRes = 0 then begin
      {New node matches a node already in the tree, free it}
      N.Free;
      RaiseContainerError(stscDupNode);
    end;
    Inc(StackP);
    with Stack[StackP] do begin
      Node := P;
      Comparison := CmpRes;
    end;
    P := P.tnPos[CmpRes > 0];
  end;

  {Insert new node}
  Stack[StackP].Node.tnPos[CmpRes > 0] := N;
  Inc(FCount);

  {Unwind the stack and rebalance}
  SubTreeInc := True;
  while (StackP > 0) and SubTreeInc do begin
    if StackP = 1 then
      InsBalance(trRoot, SubTreeInc, Stack[1].Comparison)
    else with Stack[StackP-1] do
      InsBalance(Node.tnPos[Comparison > 0], SubTreeInc, Stack[StackP].Comparison);
    dec(StackP);
  end;
end;



end.
