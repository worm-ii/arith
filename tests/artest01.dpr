{
  arith - arbitrary precision floaing point library in Delphi.
  Test program
}
{$APPTYPE CONSOLE}
program ArTest01;

uses
  SysUtils,
  Classes,
  ArKrnl,
  Test01,
  NumMsgs;

procedure TestAllOne(X,Y: TNum);
begin
  TestSum(X,Y);
  TestDiff(X,Y);
  TestProd(X,Y);
  TestQuot(X,Y);
  TestSqr(X);
  TestSqrt(Y);
  TestExp(Y);
  TestLn(X);
  TestTrig(X);
end;

procedure TestChainFraction;
var
  X,Y,Z: TNum;
  i: Integer;
begin
  X := nil;
  Y := nil;
  Z := nil;
  try
    X := TNum.Create(1000);
    X.AsInteger := 163;
    X.Sqrt_(X);
    X.Prod(X,numPi);
    X.Exp_(X);
    ReadLn;
    for i := 1 to 1000 do
    begin
      Write(X.Int64Trunc:4);
      X.Frac_(X);
      X.Quot(1,X);
    end;
  finally
    Z.Free;
    Y.Free;
    X.Free;
  end;
end;

procedure TestAll(Count: Integer);
var
  X,Y,Z: TNum;
  S: TStringList;
  i: Integer;
begin
  S := nil;
  try
    for i := 1 to Count do
    begin
      X := nil;
      Y := nil;
      Z := nil;
      try
        X := RandomNum01;
        Y := RandomNum01;
        try
          TestAllOne(X,Y);
        except
          WriteLn(i);
          WriteLn(RandSeed);
          S := TStringList.Create;
          S.AddObject('X_',X);
          S.AddObject('Y_',Y);
          CreateNumDefs('.','Fail01',S);
          raise;
        end;
      finally
        Z.Free;
        Y.Free;
        X.Free;
      end;
      Write('Done: ',i,' of ',Count,'':20,#13);
    end;
    SetDebugStage('TestChainFraction');
    TestChainFraction;
    SetDebugStage('All done');
  finally
    S.Free;
    WriteLn;
    WriteLn('Stage: ', GetDebugStage);
    ReadLn;
  end;
end;

begin
  TestAll(100);
end.
