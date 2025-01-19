{
  arith - arbitrary precision floaing point library in Delphi.
  Demo calculator
}
program EvalDemo;

uses
  Forms,
  EvalUnit in 'EvalUnit.pas' {TestForm},
  CalcHistory in 'CalcHistory.pas' {frmCalcList};

begin
  Application.CreateForm(TTestForm, TestForm);
  Application.CreateForm(TfrmCalcList, frmCalcList);
  Application.Run;
end.
