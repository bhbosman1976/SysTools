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
{* SysTools: StList.pas 4.04                             *}
{*********************************************************}
{* SysTools: Linked list class                           *}
{*********************************************************}

{$I StDefine.inc}

{Notes:
  Nodes stored in the list can be of type TStListNode or of a derived type.
  Pass the node class to the list constructor.

  TStList is a doubly-linked list that can be scanned backward just as
  efficiently as forward.

  The list retains the index and node of the last node found by Nth (or by
  the indexed array property). This makes For loops that scan a list much
  faster and speeds up random calls to Nth by about a factor of two.
}

unit StGenericList;

interface

uses
  Windows, SysUtils, Classes, StConst, StGenericBase;

type
  TStListNode<TData> = class(TStNode<TData>)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;
  protected
    FNext : TStListNode<TData>;
    FPrev : TStListNode<TData>;
  public
    constructor Create(); override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;

  TStList<TData,TNode:TStListNode<TData>> = class(TStContainer<TData>)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;

  public
    type TIterateFunc<TOtherData> = function(Container : TStList<TData,TNode>; Node : TStListNode<TData>; OtherData : TOtherData) : Boolean of object;
    class function DestroyNode(Container : TStList<TData,TNode>; Node : TStListNode<TData>; OtherData : Pointer) : Boolean;
    class function FindNode(Container : TStList<TData,TNode>; Node : TStListNode<TData>; OtherData : TData) : Boolean;
    class var ClassCritSect : TRTLCriticalSection;
    class procedure EnterClassCS;
    class procedure LeaveClassCS;
  protected
    FHead : TStListNode<TData>;
    FTail : TStListNode<TData>;
    lsLastI : Integer;
    lsLastP : TStListNode<TData>;
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create(); virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function DoCompare(Data1, Data2 : TData) : Integer;
    procedure Clear; override;
    function Append(Data : TData) : TStListNode<TData>;
    function Insert(Data : TData) : TStListNode<TData>;
    function Place(Data : Pointer; P : TStListNode<TData>) : TStListNode<TData>;
    function PlaceBefore(Data : Pointer; P : TStListNode<TData>) : TStListNode<TData>;
    function InsertSorted(Data : TData) : TStListNode<TData>;
    procedure MoveToHead(P : TStListNode<TData>);
    procedure Join(P : TStListNode<TData>; L : TStList<TData,TNode>);
    procedure Sort;
    procedure Delete(P : TStListNode<TData>);
    function Next(P : TStListNode<TData>) : TStListNode<TData>;
    function Prev(P : TStListNode<TData>) : TStListNode<TData>;
    function Nth(Index : Integer) : TStListNode<TData>;
    function NthFrom(P : TStListNode<TData>; Index : Integer) : TStListNode<TData>;
    function Posn(P : TStListNode<TData>) : Integer;
    function Distance(P1, P2 : TStListNode<TData>) : Integer;
    function Find(Data : Pointer) : TStListNode<TData>;
    function Iterate<TOtherData>(Action : TIterateFunc<TOtherData>; Up : Boolean; OtherData : TOtherData) : TStListNode<TData>;
    property Head : TStListNode<TData> read FHead;
    property Tail : TStListNode<TData> read FTail;
    property Items[Index : Integer] : TStListNode<TData> read Nth; default;
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;

{======================================================================}

implementation


class destructor TStList<TData,TNode>.Destroy;
begin

  Windows.DeleteCriticalSection(ClassCritSect);

end;

class function TStList<TData,TNode>.DestroyNode(Container : TStList<TData,TNode>; Node : TStListNode<TData>; OtherData : Pointer) : Boolean;
begin
  Container.DisposeNodeData(Node);
  Node.Free;
  Result := True;
end;

class procedure TStList<TData,TNode>.EnterClassCS;
begin

  EnterCriticalSection(TStList<TData,TNode>.ClassCritSect);

end;

class procedure TStList<TData,TNode>.LeaveClassCS;
begin

    LeaveCriticalSection(TStList<TData,TNode>.ClassCritSect);

end;

procedure TStListNode<TData>.AfterConstruction;
begin
  inherited AfterConstruction;

  InterlockedIncrement(FCreateCount);
end;

procedure TStListNode<TData>.BeforeDestruction;
begin
  InterlockedIncrement(FDestroyCount);

  inherited BeforeDestruction;
end;

constructor TStListNode<TData>.Create;
begin
  inherited Create();
end;

{----------------------------------------------------------------------}

class function TStList<TData,TNode>.FindNode(Container : TStList<TData,TNode>; Node : TStListNode<TData>; OtherData : TData) : Boolean;
begin
  Result := (Node.Data <> OtherData);
end;

procedure TStList<TData, TNode>.AfterConstruction;
begin
  inherited AfterConstruction;

  InterlockedIncrement(FCreateCount);
end;

{----------------------------------------------------------------------}

function TStList<TData,TNode>.Append(Data : TData) : TStListNode<TData>;
var
  N : TStListNode<TData>;
begin

  EnterCS;
  try

    N := TNode.create;
    N.Init(Data);
    N.FPrev := FTail;
    if not Assigned(FHead) then begin
      {Special case for first node}
      FHead := N;
      FTail := N;
    end else begin
      {Add at end of existing list}
      FTail.FNext := N;
      FTail := N;
    end;
    Inc(FCount);
    Result := N;

  finally
    LeaveCS;
  end;

end;


procedure TStList<TData, TNode>.BeforeDestruction;
begin
  InterlockedIncrement(FDestroyCount);

  inherited BeforeDestruction;
end;

procedure TStList<TData,TNode>.Clear;
begin

  EnterCS;
  try

    if Count > 0 then begin
      Iterate<Pointer>(DestroyNode, True, nil);
      FCount := 0;
    end;
    FHead := nil;
    FTail := nil;
    lsLastI := -1;
    lsLastP := nil;

  finally
    LeaveCS;
  end;

end;

class constructor TStList<TData,TNode>.Create;
begin

  Windows.InitializeCriticalSection(ClassCritSect);

end;

constructor TStList<TData,TNode>.Create();
begin
  inherited create;
  Clear;
end;

procedure TStList<TData,TNode>.Delete(P : TStListNode<TData>);
begin

  EnterCS;
  try

    if (not Assigned(P)) or (Count <= 0) then
      Exit;
    if not (P is TNode) then
      RaiseContainerError(stscBadType);

    with P do begin
      {Fix pointers of surrounding nodes}
      if Assigned(FNext) then
        FNext.FPrev := FPrev;
      if Assigned(FPrev) then
        FPrev.FNext := FNext;
    end;

    {Fix head and tail of list}
    if FTail = P then
      FTail := FTail.FPrev;
    if FHead = P then
      FHead := FHead.FNext;

    {Dispose of the node}
    DisposeNodeData(P);
    P.Free;
    Dec(FCount);
    lsLastI := -1;

  finally
    LeaveCS;
  end;

end;

function TStList<TData,TNode>.Distance(P1, P2 : TStListNode<TData>) : Integer;
var
  I : Integer;
  N : TStListNode<TData>;
begin

  EnterCS;
  try

    {Count forward}
    I := 0;
    N := P1;
    while Assigned(N) and (N <> P2) do begin
      Inc(I);
      N := N.FNext;
    end;
    if N = P2 then begin
      Result := I;
      Exit;
    end;

    {Count backward}
    I := 0;
    N := P1;
    while Assigned(N) and (N <> P2) do begin
      Dec(I);
      N := N.FPrev;
    end;
    if N = P2 then begin
      Result := I;
      Exit;
    end;

    {Not on same list}
    Result := MaxLongInt;

  finally
    LeaveCS;
  end;

end;

function TStList<TData,TNode>.Find(Data : Pointer) : TStListNode<TData>;
begin

  EnterCS;
  try

    Result := Iterate<TData>(FindNode, True, Data);

  finally
    LeaveCS;
  end;

end;


function TStList<TData,TNode>.Insert(Data : TData) : TStListNode<TData>;
var
  N : TStListNode<TData>;
begin

  EnterCS;
  try

    N := TNode.Create;
    N.Init(Data);
    {N.FPrev := nil;}
    N.FNext := FHead;
    if not Assigned(FHead) then
      {Special case for first node}
      FTail := N
    else
      {Add at start of existing list}
      FHead.FPrev := N;
    FHead := N;
    Inc(FCount);
    lsLastI := -1;
    Result := N;

  finally
    LeaveCS;
  end;

end;

function TStList<TData,TNode>.InsertSorted(Data : TData) : TStListNode<TData>;
var
  N : TStListNode<TData>;
  P : TStListNode<TData>;
begin

  EnterCS;
  try

    N := TNode.create;
    N.Init(Data);
    Result := N;
    Inc(FCount);
    lsLastI := -1;

    if not Assigned(FHead) then begin
      {First element added to list}
      FHead := N;
      FTail := N;
    end else begin
      P := FHead;
      while Assigned(P) do begin
        if DoCompare(N.Data, P.Data) < 0 then begin
          if not Assigned(P.FPrev) then begin
            {New head}
            FHead := N;
          end else begin
            P.FPrev.FNext := N;
            N.FPrev := P.FPrev;
          end;
          P.FPrev := N;
          N.FNext := P;
          Exit;
        end;
        P := P.FNext;
      end;
      {New tail}
      FTail.FNext := N;
      N.FPrev := FTail;
      FTail := N;
    end;

  finally
    LeaveCS;
  end;

end;

function TStList<TData,TNode>.Iterate<TOtherData>(Action : TIterateFunc<TOtherData>; Up : Boolean;
                         OtherData : TOtherData) : TStListNode<TData>;
var
  N : TStListNode<TData>;
  P : TStListNode<TData>;
begin

  EnterCS;
  try

    if Up then begin
      N := FHead;
      while Assigned(N) do begin
        P := N.FNext;
        if Action(Self, N, OtherData) then
          N := P
        else begin
          Result := N;
          Exit;
        end;
      end;
    end else begin
      N := FTail;
      while Assigned(N) do begin
        P := N.FPrev;
        if Action(Self, N, OtherData) then
          N := P
        else begin
          Result := N;
          Exit;
        end;
      end;
    end;
    Result := nil;

  finally
    LeaveCS;
  end;

end;

procedure TStList<TData,TNode>.Join(P : TStListNode<TData>; L : TStList<TData,TNode>);
var
  N : TStListNode<TData>;
  Q : TStListNode<TData>;
begin

  EnterClassCS;
  EnterCS;
  L.EnterCS;
  try

    if Assigned(L) then begin
      if Assigned(P) and (L.Count > 0) then begin
        {Patch the list into the current one}
        N := L.Head;
        Q := P.FNext;

        P.FNext := N;
        N.FPrev := P;

        if Assigned(Q) then begin
          N := L.Tail;
          N.FNext := Q;
          Q.FPrev := N;
        end;

        Inc(FCount, L.Count);
        lsLastI := -1;
      end;

      {Free L (but not its nodes)}
      L.IncNodeProtection;
      L.Free;
    end;

  finally
    L.LeaveCS;
    LeaveCS;
    LeaveClassCS;
  end;

end;


procedure TStList<TData,TNode>.MoveToHead(P : TStListNode<TData>);
begin

  EnterCS;
  try

    if Assigned(P) then
      if P <> Head then begin
        with P do begin
          {Fix pointers of surrounding nodes}
          if FTail = P then
            FTail := FTail.FPrev
          else
            FNext.FPrev := FPrev;
          FPrev.FNext := FNext;

          FNext := FHead;
          FPrev := nil;
        end;
        FHead.FPrev := P;
        FHead := P;
     end;

  finally
    LeaveCS;
  end;

end;

function TStList<TData,TNode>.Next(P : TStListNode<TData>) : TStListNode<TData>;
begin

  EnterCS;
  try

    Result := P.FNext;

  finally
    LeaveCS;
  end;

end;

function TStList<TData,TNode>.Nth(Index : Integer) : TStListNode<TData>;
var
  MinI : Integer;
  MinP : TStListNode<TData>;
begin

  EnterCS;
  try

    if (Index < 0) or (Index >= FCount) then
      Result := nil
    else begin
      MinI := Index;
      MinP := FHead;
      if lsLastI >= 0 then
        {scan the fewest possible nodes}
        if Index <= lsLastI then begin
          if lsLastI-Index < Index then begin
            MinI := Index-lsLastI;
            MinP := lsLastP;
          end;
        end else if Index-lsLastI < FCount-1-Index then begin
          MinI := Index-lsLastI;
          MinP := lsLastP;
        end else begin
          MinI := Index-(FCount-1);
          MinP := FTail;
        end;

      Result := NthFrom(MinP, MinI);
      lsLastI := Index;
      lsLastP := Result;
    end;

  finally
    LeaveCS;
  end;

end;

function TStList<TData,TNode>.NthFrom(P : TStListNode<TData>; Index : Integer) : TStListNode<TData>;
var
  I : Integer;
begin

  EnterCS;
  try

    if Assigned(P) then begin
      if not (P is TNode) then
        RaiseContainerError(stscBadType);
      if Index > 0 then begin
        for I := 1 to Index do begin
          P := P.FNext;
          if not Assigned(P) then
            break;
        end;
      end else begin
        for I := 1 to -Index do begin
          P := P.FPrev;
          if not Assigned(P) then
            break;
        end;
      end;
    end;
    Result := P;

  finally
    LeaveCS;
  end;

end;

function TStList<TData,TNode>.Place(Data : Pointer; P : TStListNode<TData>) : TStListNode<TData>;
var
  N : TStListNode<TData>;
begin

  EnterCS;
  try

    if not Assigned(P) then
      Result := Insert(Data)
    else if P = FTail then
      Result := Append(Data)
    else begin
      N := TNode.Create;
      N.Init(Data);
      N.FPrev := P;
      N.FNext := P.FNext;
      P.FNext.FPrev := N;
      P.FNext := N;
      Inc(FCount);
      lsLastI := -1;
      Result := N;
    end;

  finally
    LeaveCS;
  end;

end;

function TStList<TData,TNode>.PlaceBefore(Data : Pointer; P : TStListNode<TData>) : TStListNode<TData>;
var
  N : TStListNode<TData>;
begin

  EnterCS;
  try

    if (not Assigned(P)) or (P = Head) then
      {Place the new element at the start of the list}
      Result := Insert(Data)
    else begin
      {Patch in the new element}
      N := TNode.create;
      N.Init(Data);
      N.FNext := P;
      N.FPrev := P.FPrev;
      P.FPrev.FNext := N;
      P.FPrev := N;
      lsLastI := -1;
      Inc(FCount);
      Result := N;
    end;

  finally
    LeaveCS;
  end;

end;

function TStList<TData,TNode>.Posn(P : TStListNode<TData>) : Integer;
var
  I : Integer;
  N : TStListNode<TData>;
begin

  EnterCS;
  try

    if not Assigned(P) then
      Result := -1
    else begin
      if not (P is TNode) then
        RaiseContainerError(stscBadType);
      I := 0;
      N := FHead;
      while Assigned(N) do begin
        if P = N then begin
          Result := I;
          exit;
        end;
        Inc(I);
        N := N.FNext;
      end;
      Result := -1;
    end;

  finally
    LeaveCS;
  end;

end;

function TStList<TData,TNode>.Prev(P : TStListNode<TData>) : TStListNode<TData>;
begin

  EnterCS;
  try

    Result := P.FPrev;

  finally
    LeaveCS;
  end;

end;

procedure TStList<TData,TNode>.Sort;
const
  StackSize = 32;
type
  Stack = array[0..StackSize-1] of TStListNode<TData>;
var
  L : TStListNode<TData>;
  R : TStListNode<TData>;
  PL : TStListNode<TData>;
  PR : TStListNode<TData>;
  PivotData : TData;
  TmpData : TData;
  Dist : Integer;
  DistL : Integer;
  DistR : Integer;
  StackP : Integer;
  LStack : Stack;
  RStack : Stack;
  DStack : array[0..StackSize-1] of Integer;
begin

  EnterCS;
  try

    {Need at least 2 elements to sort}
    if Count <= 1 then
      Exit;
    lsLastI := -1;

    {Initialize the stacks}
    StackP := 0;
    LStack[0] := FHead;
    RStack[0] := FTail;
    DStack[0] := Count-1;

    {Repeatedly take top partition from stack}
    repeat

      {Pop the stack}
      L := LStack[StackP];
      R := RStack[StackP];
      Dist := DStack[StackP];
      Dec(StackP);

      if L <> R then
        {Sort current partition}
        repeat

          {Load the pivot element}
          PivotData := NthFrom(L, Dist div 2).Data;
          PL := L;
          PR := R;
          DistL := Dist;
          DistR := Dist;

          {Swap items in sort order around the pivot index}
          repeat
            while DoCompare(PL.Data, PivotData) < 0 do begin
              PL := PL.FNext;
              Dec(Dist);
              Dec(DistR);
            end;
            while DoCompare(PivotData, PR.Data) < 0 do begin
              PR := PR.FPrev;
              Dec(Dist);
              Dec(DistL);
            end;
            if Dist >= 0 then begin
              if PL <> PR then begin
                {Swap the two elements}
                TmpData := PL.Data;
                PL.Data := PR.Data;
                PR.Data := TmpData;
              end;
              if Assigned(PL.FNext) then begin
                PL := PL.FNext;
                Dec(Dist);
                Dec(DistR);
              end;
              if Assigned(PR.FPrev) then begin
                PR := PR.FPrev;
                Dec(Dist);
                Dec(DistL);
              end;
            end;
          until Dist < 0;

          {Decide which partition to sort next}
          if DistL < DistR then begin
            {Right partition is bigger}
            if DistR > 0 then begin
              {Stack the request for sorting right partition}
              Inc(StackP);
              LStack[StackP] := PL;
              RStack[StackP] := R;
              DStack[StackP] := DistR;
            end;
            {Continue sorting left partition}
            R := PR;
            Dist := DistL;
          end else begin
            {Left partition is bigger}
            if DistL > 0 then begin
              {Stack the request for sorting left partition}
              Inc(StackP);
              LStack[StackP] := L;
              RStack[StackP] := PR;
              DStack[StackP] := DistL;
            end;
            {Continue sorting right partition}
            L := PL;
            Dist := DistR;
          end;

        until Dist <= 0;
    until StackP < 0;

  finally
    LeaveCS;
  end;

end;



function TStList<TData,TNode>.DoCompare(Data1, Data2 : TData) : Integer;
begin
  Result := 0;
  if Assigned(FOnCompare) then
    FOnCompare(Self, Data1, Data2, Result)
  else if Assigned(FCompare) then
    Result := FCompare(Data1, Data2);
end;

end.


