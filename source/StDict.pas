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
{* SysTools: StDict.pas 4.04                             *}
{*********************************************************}
{* SysTools: Dictionary class                            *}
{*********************************************************}

{$I StDefine.inc}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{Notes:
  Nodes stored in the dictionary must be of type TStDictNode.

  Duplicate strings are not allowed in the dictionary.

  Calling Exists moves the found node to the front of its hash bin list.

  Iterate scans the nodes in hash order.

  Hashing and comparison is case-insensitive by default.

  In 16-bit mode, HashSize must be in the range 1..16380. In 32-bit
  mode, there is no practical limit on HashSize. A particular value
  of HashSize may lead to a better distribution of symbols in the
  dictionary, and therefore to better performance. Generally HashSize
  should be about the same size as the number of symbols expected in
  the dictionary. A prime number tends to give a better distribution.
  Based on analysis by D. Knuth, the following values are good
  choices for HashSize when the dictionary keys are alphanumeric
  strings:

    59 61 67 71 73 127 131 137 191 193 197 199 251 257 263 311 313
   317 379 383 389 439 443 449 457 503 509 521 569 571 577 631 641
   643 647 701 709 761 769 773 823 827 829 839 887 953 967

  Good values for larger tables can be computed by the GOODHASH.PAS
  bonus program.
}

unit StDict;

interface

uses
  Windows, SysUtils, Classes,
  StConst, StBase,
  System.Generics.Defaults;

type
  TStDictNode = class(TStNode)
{.Z+}
    protected
      dnNext : TStDictNode;     {Next node in hash list}
      dnName : string;          {Name of symbol, already a pointer}
      function GetName : string;

{.Z-}
    public
      constructor CreateStr(const Name : string; AData : Pointer);
        {-Initialize node}
      destructor Destroy; override;
        {-Free name string and destroy node}

      property Name : string
         read GetName;
  end;




  TStBaseDictionary = class(TStContainer)
  end;

  TStDictionary = class(TStBaseDictionary)
  private
  {$IFDEF ThreadSafe}
  class var ClassCritSect : TRTLCriticalSection;
  {$ENDIF}
  public
    type
      TSymbolArray = array[0..(StMaxBlockSize div SizeOf(TStDictNode))-1] of TStDictNode;
      PSymbolArray = ^TSymbolArray;
      TIterateFunc<TOtherData> = function(Container : TStDictionary; Node : TStDictNode; OtherData : TOtherData) : Boolean of object;
  public
    class procedure EnterClassCS;
    class procedure LeaveClassCS;
    class function DestroyNode(Container : TStDictionary; Node : TStDictNode; OtherData : Pointer) : Boolean;
    class function FindNodeData(Container : TStDictionary; Node : TStDictNode; OtherData : Pointer) : Boolean;
    class function JoinNode(Container : TStDictionary; Node : TStDictNode; OtherData : TStDictionary) : Boolean;
  private
    function CalculateHash(const s: string): integer;
{.Z+}
  protected
    FComparer: System.Generics.Defaults.IComparer<String>;
    FEqualityComparer: System.Generics.Defaults.IEqualityComparer<String>;
    {property instance variables}
    FHashSize : Integer;            {Bins in symbol array}





    {private instance variables}
    dySymbols : PSymbolArray;     {Pointer to symbol array}
    dyIgnoreDups : Boolean;       {Ignore duplicates during Join?}



    procedure dySetHashSize(Size : Integer);
    procedure dyFindNode(const Name : string; var H : Integer; var Prev, This : TStDictNode);
{.Z-}
  public
    class constructor create;
    class destructor Destroy;
    constructor Create(AHashSize : Integer); virtual;
      {-Initialize an empty dictionary}
    destructor Destroy; override;
      {-Destroy a dictionary}


    procedure Clear; override;
      {-Remove all nodes from container but leave it instantiated}
    function Exists(const Name : string; var Data : Pointer) : Boolean;
      {-Return True and the Data pointer if Name is in the dictionary}
    procedure Add(const Name : string; Data : Pointer);
      {-Add new Name and Data to the dictionary}
    procedure Delete(const Name : string);
      {-Delete a Name from the dictionary}
    function Find(Data : Pointer; var Name : string) : Boolean;
      {-Return True and the element Name that matches Data}

    procedure Join(D : TStDictionary; IgnoreDups : Boolean);
      {-Add dictionary D into this one and dispose D}

    function Iterate<TOtherData>(Action : TIterateFunc<TOtherData>; OtherData : TOtherData) : TStDictNode;
      {-Call Action for all the nodes, returning the last node visited}

    function BinCount(H : Integer) : Integer;
      {-Return number of names in a hash bin (for testing)}

    property HashSize : Integer
      read FHashSize
      write dySetHashSize;

  end;
implementation

class constructor TStDictionary.create;
begin
  {$IFDEF ThreadSafe}
  Windows.InitializeCriticalSection(ClassCritSect);
  {$ENDIF}
end;

class destructor TStDictionary.Destroy;
begin
  {$IFDEF ThreadSafe}
  Windows.DeleteCriticalSection(ClassCritSect);
  {$ENDIF}
end;

class function TStDictionary.DestroyNode(Container : TStDictionary; Node : TStDictNode; OtherData : Pointer) : Boolean;
begin
  Container.DisposeNodeData(Node);
  Node.Free;
  Result := True;
end;

function TStDictionary.CalculateHash(const s: String): integer;
begin
  Result := FEqualityComparer.GetHashCode(s) and $7FFFFFFF mod FHashSize;
end;

class procedure TStDictionary.EnterClassCS;
begin
{$IFDEF ThreadSafe}
  EnterCriticalSection(ClassCritSect);
{$ENDIF}
end;

class procedure TStDictionary.LeaveClassCS;
begin
{$IFDEF ThreadSafe}
  LeaveCriticalSection(ClassCritSect);
{$ENDIF}
end;

constructor TStDictNode.CreateStr(const Name : string; AData : Pointer);
begin
  Create(AData);
  dnName := Name;
end;

destructor TStDictNode.Destroy;
begin
  dnName := '';
  inherited Destroy;
end;

function TStDictNode.GetName : string;
begin
  Result := dnName;
end;



class function TStDictionary.FindNodeData(Container : TStDictionary; Node : TStDictNode; OtherData : Pointer) : Boolean;
begin
  Result := (OtherData <> Node.Data);
end;

class function TStDictionary.JoinNode(Container : TStDictionary;Node : TStDictNode; OtherData : TStDictionary) : Boolean;
var
  H : Integer;
  P, T : TStDictNode;
begin
  Result := True;
  with TStDictionary(OtherData) do begin
    dyFindNode(TStDictNode(Node).dnName, H, P, T);
    if Assigned(T) then
      if dyIgnoreDups then begin
        Node.Free;
        Exit;
      end else
        RaiseContainerError(stscDupNode);
    T := dySymbols^[H];
    dySymbols^[H] := TStDictNode(Node);
    dySymbols^[H].dnNext := T;
    Inc(FCount);
  end;
end;

function AssignNode(Container : TStContainer;
                    Node : TStNode;
                    OtherData : Pointer) : Boolean; far;
  var
    DictNode : TStDictNode absolute Node;
    OurDict : TStDictionary absolute OtherData;
  begin
    OurDict.Add(DictNode.Name, DictNode.Data);
    Result := true;
  end;

{----------------------------------------------------------------------}

procedure TStDictionary.Add(const Name : string; Data : Pointer);
var
  H : Integer;
  P, T : TStDictNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    dyFindNode(Name, H, P, T);
    if Assigned(T) then
      RaiseContainerError(stscDupNode);
    T := dySymbols^[H];
    dySymbols^[H] := TStDictNode.CreateStr(Name, Data);
    dySymbols^[H].dnNext := T;
    Inc(FCount);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;


function TStDictionary.BinCount(H : Integer) : Integer;
var
  C : Integer;
  T : TStDictNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    C := 0;
    T := dySymbols^[H];
    while Assigned(T) do begin
      inc(C);
      T := T.dnNext;
    end;
    Result := C;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStDictionary.Clear;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if FCount <> 0 then begin
      Iterate<Pointer>(DestroyNode, nil);
      FCount := 0;
      FillChar(dySymbols^, Integer(FHashSize)*SizeOf(TStDictNode), 0);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

constructor TStDictionary.Create(AHashSize : Integer);
begin
  CreateContainer(TStDictNode, 0);
  FComparer := System.Generics.Defaults.IComparer<String>(TComparer<String>._Default);
  FEqualityComparer := System.Generics.Defaults.IEqualityComparer<String>(TEqualityComparer<String>._Default);
  HashSize := AHashSize;
end;

procedure TStDictionary.Delete(const Name : string);
var
  H : Integer;
  P, T : TStDictNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    dyFindNode(Name, H, P, T);
    if Assigned(T) then begin
      if Assigned(P) then
        P.dnNext := T.dnNext
      else
        dySymbols^[H] := T.dnNext;
      DestroyNode(Self, T, nil);
      Dec(FCount);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

destructor TStDictionary.Destroy;
begin
  if conNodeProt = 0 then
    Clear;
  if Assigned(dySymbols) then
    FreeMem(dySymbols, Integer(FHashSize)*SizeOf(TStDictNode));
  IncNodeProtection;
  inherited Destroy;
end;


procedure TStDictionary.dyFindNode(const Name : string; var H : Integer;
                                   var Prev, This : TStDictNode);
var
  P, T : TStDictNode;
begin
  Prev := nil;
  This := nil;
  H := FEqualityComparer.GetHashCode(Name);
  T := dySymbols^[H];
  P := nil;
  while Assigned(T) do begin
    if FComparer.Compare(Name, T.dnName) = 0 then begin
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

procedure TStDictionary.dySetHashSize(Size : Integer);
var
  H, OldSize :  Integer;
  TableSize : Integer;
  T, N : TStDictNode;
  OldSymbols : PSymbolArray;
  OldDisposeData : TDisposeDataProc;
  OldOnDisposeData : TStDisposeDataEvent;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    TableSize := Integer(Size)*SizeOf(TStDictNode);
    if (Size <= 0) {or (TableSize > MaxBlockSize)} then
      RaiseContainerError(stscBadSize);

    if Size <> FHashSize then begin
      OldSymbols := dySymbols;
      OldSize := FHashSize;

      {Get a new hash table}
      GetMem(dySymbols, TableSize);
      FillChar(dySymbols^, TableSize, 0);
      FCount := 0;
      FHashSize := Size;

      if OldSize <> 0 then begin
        {Prevent disposing of the user data while transferring elements}
        OldDisposeData := DisposeData;
        DisposeData := nil;
        OldOnDisposeData := OnDisposeData;
        OnDisposeData := nil;
        {Add old symbols into new hash table}
        for H := 0 to OldSize-1 do begin
          T := OldSymbols^[H];
          while Assigned(T) do begin
            Add(T.dnName, T.Data);
            N := T.dnNext;
            {free the node just transferred}
            T.Free;
            T := N;
          end;
        end;
        {Dispose of old hash table}
        FreeMem(OldSymbols, OldSize*SizeOf(TStDictNode));
        {Reassign the dispose data routine}
        DisposeData := OldDisposeData;
        OnDisposeData := OldOnDisposeData;
      end;

      {FHashSize := Size;}
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStDictionary.Exists(const Name : String; var Data : Pointer) : Boolean;
var
  H : Integer;
  P, T : TStDictNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
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
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStDictionary.Find(Data : Pointer; var Name : string) : Boolean;
var
  T : TStDictNode;
begin
  Name := '';
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    T := Iterate<Pointer>(FindNodeData, Data);
    if Assigned(T) then begin
      Result := True;
      Name := T.dnName;
    end else
      Result := False;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;


function TStDictionary.Iterate<TOtherData>(Action : TIterateFunc<TOtherData>;
                               OtherData : TOtherData) : TStDictNode;
var
  H    : Integer;
  T, N : TStDictNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
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
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStDictionary.Join(D : TStDictionary; IgnoreDups : Boolean);
begin
{$IFDEF ThreadSafe}
  EnterClassCS;
  EnterCS;
  D.EnterCS;
  try
{$ENDIF}
    dyIgnoreDups := IgnoreDups;
    D.Iterate<TStDictionary>(JoinNode, Self);

    {Dispose of D, but not its nodes}
    D.IncNodeProtection;
    D.Free;
{$IFDEF ThreadSafe}
  finally
    D.LeaveCS;
    LeaveCS;
    LeaveClassCS;
  end;
{$ENDIF}
end;


end.
