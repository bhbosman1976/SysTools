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

unit StTree;

interface

uses
  Windows,
  SysUtils, Classes, StConst, StBase;

type
  TStTreeNode = class(TStNode)
  {.Z+}
    protected
      tnPos  : array[Boolean] of TStTreeNode; {Child nodes}
      tnBal  : Integer;         {Used during balancing}

  {.Z-}
    public
      constructor Create(AData : Pointer); override;

  end;
  TStTree = class(TStContainer)
  public
    type TIterateFunc = function(Container : TStTree; Node : TStTreeNode; OtherData : Pointer) : Boolean of object;
    class function DestroyNode(Container : TStTree; Node : TStTreeNode; OtherData : Pointer) : Boolean;
    class function JoinNode(Container : TStTree; Node : TStTreeNode; OtherData : Pointer) : Boolean;
    class function SplitTree(Container : TStTree; Node : TStTreeNode; OtherData : Pointer) : Boolean;
    {$IFDEF ThreadSafe}
    class var ClassCritSect : TRTLCriticalSection;
    {$ENDIF}
    class procedure EnterClassCS;
    class procedure LeaveClassCS;
  protected
    trRoot : TStTreeNode;       {Root of tree}
    trIgnoreDups : Boolean;     {Ignore duplicates during Join?}
    procedure trInsertNode(N : TStTreeNode);
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create(NodeClass : TStNodeClass); virtual;
    {-Initialize an empty tree}
    function DoCompare(Data1, Data2 : Pointer) : Integer;

    procedure Clear; override;
    {-Remove all nodes from container but leave it instantiated}

    function Insert(Data : Pointer) : TStTreeNode;
    {-Add a new node}
    procedure Delete(Data : Pointer);
    {-Delete a node}
    function Find(Data : Pointer) : TStTreeNode;
    {-Return node that matches Data}

    procedure Join(T: TStTree; IgnoreDups : Boolean);
    {-Add tree T into this one and dispose T}
    function Split(Data : Pointer) : TStTree;
    {-Split tree, putting all nodes above and including Data into new tree}

    function Iterate(Action : TIterateFunc; Up : Boolean;
                   OtherData : Pointer) : TStTreeNode;
    {-Call Action for all the nodes, returning the last node visited}

    function First : TStTreeNode;
    {-Return the smallest-value node in the tree}
    function Last : TStTreeNode;
    {-Return the largest-value node in the tree}
    function Next(N : TStTreeNode) : TStTreeNode;
    {-Return the next node whose value is larger than N's}
    function Prev(N : TStTreeNode) : TStTreeNode;
    {-Return the largest node whose value is smaller than N's}
  end;

{.Z+}
  TStTreeClass = class of TStTree;
{.Z-}

{======================================================================}

implementation


class destructor TStTree.Destroy;
begin
  {$IFDEF ThreadSafe}
  Windows.DeleteCriticalSection(ClassCritSect);
  {$ENDIF}
end;

class function TStTree.DestroyNode(Container : TStTree; Node : TStTreeNode; OtherData : Pointer) : Boolean;
begin
  Container.DisposeNodeData(Node);
  Node.Free;
  Result := True;
end;

class procedure TStTree.EnterClassCS;
begin
{$IFDEF ThreadSafe}
  EnterCriticalSection(ClassCritSect);
{$ENDIF}
end;

class procedure TStTree.LeaveClassCS;
begin
{$IFDEF ThreadSafe}
  LeaveCriticalSection(ClassCritSect);
{$ENDIF}
end;

const
  Left = False;
  Right = True;

{Following stack declarations are used to avoid recursion in all tree
 routines. Because the tree is AVL-balanced, a stack size of 40
 allows at least 2**32 elements in the tree without overflowing the
 stack.}

const
  StackSize = 40;

type
  StackNode =
    record
      Node : TStTreeNode;
      Comparison : Integer;
    end;
  StackArray = array[1..StackSize] of StackNode;

constructor TStTreeNode.Create(AData : Pointer);
begin
  inherited Create(AData);
end;



function TStTree.DoCompare(Data1, Data2 : Pointer) : Integer;
begin
  Result := 0;
  if Assigned(FOnCompare) then
    FOnCompare(Self, Data1, Data2, Result)
  else if Assigned(FCompare) then
    Result := FCompare(Data1, Data2);
end;


{----------------------------------------------------------------------}

function Sign(I : Integer) : Integer;
begin
  if I < 0 then
    Sign := -1
  else if I > 0 then
    Sign := +1
  else
    Sign := 0;
end;

procedure DelBalance(var P : TStTreeNode; var SubTreeDec : Boolean; CmpRes : Integer);
var
  P1, P2 : TStTreeNode;
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

procedure InsBalance(var P : TStTreeNode; var SubTreeInc : Boolean;
                     CmpRes : Integer);
var
  P1 : TStTreeNode;
  P2 : TStTreeNode;
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

class function TStTree.JoinNode(Container : TStTree; Node : TStTreeNode; OtherData : Pointer) : Boolean;
var
  N : TStTreeNode;
begin
  Result := True;
  N := TStTree(OtherData).Find(Node.Data);
  if Assigned(N) then
    if TStTree(OtherData).trIgnoreDups then begin
      Node.Free;
      Exit;
    end else
      RaiseContainerError(stscDupNode);

  with TStTreeNode(Node) do begin
    tnPos[Left] := nil;
    tnPos[Right] := nil;
    tnBal := 0;
  end;
  TStTree(OtherData).trInsertNode(TStTreeNode(Node));
end;

type
  SplitRec =
  record
    SData : Pointer;
    STree : TStTree;
  end;

class function TStTree.SplitTree(Container : TStTree; Node : TStTreeNode; OtherData : Pointer) : Boolean;
var
  D : Pointer;
begin
  Result := True;
  if Container.DoCompare(Node.Data, SplitRec(OtherData^).SData) >= 0 then begin
    D := Node.Data;
    TStTree(Container).Delete(D);
    SplitRec(OtherData^).STree.Insert(D);
  end;
end;

{----------------------------------------------------------------------}

procedure TStTree.Clear;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if conNodeProt = 0 then
      Iterate(DestroyNode, True, nil);
    trRoot := nil;
    FCount := 0;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;



constructor TStTree.Create(NodeClass : TStNodeClass);
begin
  CreateContainer(NodeClass, 0);
end;

class constructor TStTree.Create;
begin
  {$IFDEF ThreadSafe}
  Windows.InitializeCriticalSection(ClassCritSect);
  {$ENDIF}
end;

procedure TStTree.Delete(Data : Pointer);
var
  P : TStTreeNode;
  Q : TStTreeNode;
  TmpData : Pointer;
  CmpRes : Integer;
  Found : Boolean;
  SubTreeDec : Boolean;
  StackP : Integer;
  Stack : StackArray;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
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
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStTree.Find(Data : Pointer) : TStTreeNode;
var
  P : TStTreeNode;
  CmpRes : Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
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
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStTree.First : TStTreeNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count = 0 then
      Result := nil
    else begin
      Result := trRoot;
      while Assigned(Result.tnPos[Left]) do
        Result := Result.tnPos[Left];
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStTree.Insert(Data : Pointer) : TStTreeNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {Create the node}
    Result := TStTreeNode(conNodeClass.Create(Data));
    trInsertNode(Result);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStTree.Iterate(Action : TIterateFunc; Up : Boolean;
                         OtherData : Pointer) : TStTreeNode;
var
  P : TStTreeNode;
  Q : TStTreeNode;
  StackP : Integer;
  Stack : StackArray;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
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
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStTree.Join(T: TStTree; IgnoreDups : Boolean);
begin
{$IFDEF ThreadSafe}
  EnterClassCS;
  EnterCS;
  T.EnterCS;
  try
{$ENDIF}
    trIgnoreDups := IgnoreDups;
    T.Iterate(JoinNode, True, Self);
    T.IncNodeProtection;
    T.Free;
{$IFDEF ThreadSafe}
  finally
    T.LeaveCS;
    LeaveCS;
    LeaveClassCS;
  end;
{$ENDIF}
end;

function TStTree.Last : TStTreeNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count = 0 then
      Result := nil
    else begin
      Result := trRoot;
      while Assigned(Result.tnPos[Right]) do
        Result := Result.tnPos[Right];
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStTree.Next(N : TStTreeNode) : TStTreeNode;
var
  Found : Word;
  P : TStTreeNode;
  StackP : Integer;
  Stack : StackArray;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
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
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStTree.Prev(N : TStTreeNode) : TStTreeNode;
var
  Found : Word;
  P : TStTreeNode;
  StackP : Integer;
  Stack : StackArray;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
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
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStTree.Split(Data : Pointer) : TStTree;
var
  SR : SplitRec;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {Create and initialize the new tree}
    Result := TStTreeClass(ClassType).Create(conNodeClass);
    Result.Compare := Compare;
    Result.OnCompare := OnCompare;
    Result.DisposeData := DisposeData;
    Result.OnDisposeData := OnDisposeData;

    {Scan all elements to transfer some to new tree}
    SR.SData := Data;
    SR.STree := Result;
    {Prevent SplitTree from disposing of node data it moves from old tree to new}
    DisposeData := nil;
    OnDisposeData := nil;
    Iterate(SplitTree, True, @SR);
    {Restore DisposeData property}
    DisposeData := Result.DisposeData;
    OnDisposeData := Result.OnDisposeData;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStTree.trInsertNode(N : TStTreeNode);
var
  P : TStTreeNode;
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
