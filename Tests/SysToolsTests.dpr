program SysToolsTests;
{

  Delphi DUnit-Testprojekt
  -------------------------
  Dieses Projekt enthält das DUnit-Test-Framework und die GUI/Konsolen-Test-Runner.
  Fügen Sie den Bedingungen in den Projektoptionen "CONSOLE_TESTRUNNER" hinzu,
  um den Konsolen-Test-Runner zu verwenden.  Ansonsten wird standardmäßig der
  GUI-Test-Runner verwendet.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  TestStBase in 'TestStBase.pas',
  StGenericBase in '..\source\StGenericBase.pas',
  StGenericTree in '..\source\StGenericTree.pas',
  StGenericList in '..\source\StGenericList.pas',
  StGenericColl in '..\source\StGenericColl.pas',
  StGenericDict in '..\source\StGenericDict.pas',
  StDenericDQue in '..\source\StDenericDQue.pas',
  TestStUtils in 'TestStUtils.pas',
  TestGenerics in 'TestGenerics.pas',
  TestStStrL in 'TestStStrL.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  DUnitTestRunner.RunRegisteredTests;
end.

