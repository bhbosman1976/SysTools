unit TestGenerics;

interface

uses
  SysUtils, TestFramework;

type
  TTestColl = class(TTestCase)
  private
  published
    procedure TestColl;
    procedure TestDict1;
    procedure TestDict2;
  end;

implementation

uses
  StGenericColl, StGenericDict;


{ TTestColl }

procedure TTestColl.TestColl;
begin
  var LColl := TStCollection.Create(100);
  try
    LColl.Insert(Pointer(1));
    LColl.Insert(Pointer(2));
    LColl.Insert(Pointer(3));
    LColl.Insert(Pointer(4));
    LColl.Insert(Pointer(5));
  finally
    LColl.free;
  end;
end;

procedure TTestColl.TestDict1;
begin
  var LDict := TStDictionary<string, Pointer>.Create(101).Create(101);
  try
    LDict.Add('a', nil);
    LDict.Add('a', nil);

  finally
    LDict.free;
  end;

end;

procedure TTestColl.TestDict2;
begin
  var LDict := TStDictionary<string, Pointer>.Create(101);
  try
    LDict.Add('a', nil);
    LDict.Add('a', nil);

  finally
    LDict.free;
  end;
end;

initialization
  RegisterTest(TTestColl.Suite);

end.

