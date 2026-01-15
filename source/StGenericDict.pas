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

unit StGenericDict;

interface

uses
  Windows, SysUtils, Classes,
  StConst, StGenericBase,
  System.Generics.Defaults;

type
  TStDictNode<TKey,TData> = class(TStNode<TData>)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;
  protected
    dnNext : TStDictNode<TKey,TData>;
    FKey : TKey;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Init(const AKey: TKey; const AData: TData); reintroduce; virtual;
    property Key : TKey read FKey;
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;

  TStDictionary<TKey,TData> = class(TStContainer<TData>)
  strict private
    class var FDestroyCount: integer;
    class var FCreateCount: integer;
  private
    class var ClassCritSect : TRTLCriticalSection;
  public
    type
      TSymbolArray = array[0..(StMaxBlockSize div 8)-1] of TStDictNode<TKey,TData>;
      PSymbolArray = ^TSymbolArray;
      TIterateFunc<TOtherData> = function(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : TOtherData) : Boolean of object;
  public
    class procedure EnterClassCS;
    class procedure LeaveClassCS;
    class function DestroyNode(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : Pointer) : Boolean;
    class function FindNodeData(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : TData) : Boolean;
  protected
    FOnDisposeKey : TStDisposeEvent<TKey>;
    FDisposeKey : TDisposeProc<TKey>;
    FComparer: System.Generics.Defaults.IComparer<TKey>;
    FEqualityComparer: System.Generics.Defaults.IEqualityComparer<TKey>;
    FHashSize : Integer;
    dySymbols : PSymbolArray;
    dyIgnoreDups : Boolean;
    procedure dySetHashSize(Size : Integer);
    procedure dyFindNode(const Name : TKey; var H : Integer; var Prev, This : TStDictNode<TKey,TData>);
    procedure DisposeNodeKey(const P: TStDictNode<TKey,TData>);
  public
    class constructor create;
    class destructor Destroy;
    constructor Create(AHashSize : Integer); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Clear; override;
    function Exists(const Name : TKey; var Data : TData) : Boolean;
    procedure Add(const Name : TKey; Data : TData);
    procedure Delete(const Name : TKey);
    function Find(Data : TData; var Name : TKey) : Boolean;
    function Iterate<TOtherData>(Action : TIterateFunc<TOtherData>; OtherData : TOtherData) : TStDictNode<TKey,TData>;
    function BinCount(H : Integer) : Integer;
    procedure DoDisposeKey(const AKey : TKey); virtual;
    property HashSize : Integer read FHashSize;
    property DisposeKey : TDisposeProc<TKey> read FDisposeKey write FDisposeKey;
    property OnDisposeKey : TStDisposeEvent<TKey> read FOnDisposeKey write FOnDisposeKey;
    class property CreateCount: integer read FCreateCount write FCreateCount;
    class property DestroyCount: integer read FDestroyCount write FDestroyCount;
  end;

implementation

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

procedure TStDictNode<TKey,TData>.Init(const AKey: TKey; const AData: TData);
begin
  inherited Init(AData);
  FKey := AKey;
end;

class function TStDictionary<TKey,TData>.FindNodeData(Container : TStDictionary<TKey,TData>; Node : TStDictNode<TKey,TData>; OtherData : TData) : Boolean;
begin
  Result := (OtherData <> Node.Data);
end;

{----------------------------------------------------------------------}

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
    dySymbols^[H] := TStDictNode<TKey,TData>.create;
    dySymbols^[H].Init(Name, Data);
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

procedure TStDictionary<TKey,TData>.Clear;
begin
  EnterCS;
  try
    if FCount <> 0 then begin
      Iterate<Pointer>(DestroyNode, nil);
      FCount := 0;
      FillChar(dySymbols^, Integer(FHashSize)*SizeOf(TStDictNode<TKey,TData>), 0);
    end;
  finally
    LeaveCS;
  end;
end;

constructor TStDictionary<TKey,TData>.Create(AHashSize : Integer);
begin
  Create(0);
  FComparer := System.Generics.Defaults.IComparer<TKey>(TComparer<TKey>._Default);
  FEqualityComparer := System.Generics.Defaults.IEqualityComparer<TKey>(TEqualityComparer<TKey>._Default);
  dySetHashSize(AHashSize);
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
  H := FEqualityComparer.GetHashCode(Name) and $7FFFFFFFmod FHashSize ;
  T := dySymbols^[H];
  P := nil;
  while Assigned(T) do begin
    if FComparer.Compare(Name, T.Key) = 0 then begin
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
    T := Iterate<TData>(FindNodeData, Data);
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

end.
