{
  arith - arbitrary precision floaing point library in Delphi.
  Test unit 1
}
unit Test01;

interface

uses ArKrnl, ArStacks;

var
  MaxTestDecPrec: Integer = 700;

procedure SetDebugStage(const Stage: string);
function GetDebugStage: string;

function RandomSign: TSign;
function Equals(N1,N2: TNum): Boolean;
procedure CheckEquals(X,Y: TNum);
function TestNum(N: TNum): Boolean;
procedure CheckNum(X: TNum);
function RandomPrec: TDecPrec;
function RandomNum01: TNum;
procedure TestSum(X,Y: TNum; R: TNum = nil);
procedure TestDiff(X,Y: TNum; R: TNum = nil);
procedure TestProd(X,Y: TNum; R: TNum = nil);
procedure TestQuot(X,Y: TNum; R: TNum = nil);
procedure TestSqr(X: TNum);
procedure TestSqrt(X: TNum);
procedure TestLn(X: TNum);
procedure TestExp(X: TNum);
procedure TestTrig(X: TNum);

procedure TestIntPower(X: TNum; Pow: Integer);
procedure TestValidateDecs(X: TNum);


implementation

uses Const_00;

var
  DebugStage: string = '';

procedure SetDebugStage(const Stage: string);
begin
  DebugStage := Stage;
end;

function GetDebugStage: string;
begin
  Result := DebugStage;
end;

type
  TQWordRec = array[0..3] of Word;

function RandomQWord: QWord;
var
  i: Integer;
begin
  for i := 0 to 3 do TQWordRec(Result)[i] := Random(65536);
end;

function RandomSign: TSign;
begin
  Result := SignOfInt(Random(2)*2-1);
end;

function Equals(N1,N2: TNum): Boolean;
var
  D: TNum;
  Prec,AExp: Integer;
begin
  Prec := IntMin(N1.DecPrec,N2.DecPrec);
  D := TNum.Create(MinDecPrec);
  try
    D.Diff(N1,N2);
    Result := True;
    if D.Sign = Zero then Exit;
    D.Sign := Positive;
    if N1.Sign = Zero
    then AExp := N2.BinExp
    else if N2.Sign = Zero
         then AExp := N1.BinExp
         else AExp := IntMax(N1.BinExp,N2.BinExp);
    Result := D.BinExp < AExp - Trunc(Prec/Log_2_10) - 1;
  finally
    D.Free;
  end;
end;

procedure CheckEquals(X,Y: TNum);
begin
  if not Equals(X,Y) then raise ENum.Create('Numbers are not equal: ' + X.AsString + ' <> ' + Y.AsString);
end;

function TestNum(N: TNum): Boolean;
var
  Z: Boolean;
begin
  with N do
  begin
    Result :=
         (N <> nil)
     and (DecPrec >= Low(TDecPrec))
     and (DecPrec <= High(TDecPrec))
     //and (DecsValid or QWordsValid)
     and (Sign in [Low(TSign)..High(TSign)])
     and (DecExp <= MaxDecExp)
     and (DecExp >= -MaxDecExp)
     and (BinExp <= MaxBinExp)
     and (BinExp >= -MaxBinExp);
    if not Result then Exit;
    Z := Sign = Zero;
    Result :=
         (Z = (DecDigits[0] = 0))
     and (Z = (QWordDigits[QWordPrec-1].Hi = 0))
     and (Z <> (QWordDigits[QWordPrec-1].Hi < 0))
     and (Z <= (BinExp = 0))
     and (Z <= (DecExp = 0));
  end;
end;

procedure CheckNum(X: TNum);
begin
  if not TestNum(X) then raise ENum.Create('Number is invalid');
end;

function RandomPrec: TDecPrec;
begin
  Result := MinDecPrec + Random(MaxTestDecPrec-MinDecPrec+1);
end;

function RandomNum01: TNum;
var
  i: Integer;
  NextQWord: QWord;
begin
  Result := TNum.Create(RandomPrec);
  try
    Result.Sign := RandomSign;
    for i := 0 to Result.QWordPrec - 1 do
    begin
      NextQWord := RandomQWord;
      if i = Result.QWordPrec - 1 then NextQWord.Hi := NextQWord.Hi or MinDWord;
      Result.QWordDigits[i] := NextQWord;
    end;
    CheckNum(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TestSum(X,Y: TNum; R: TNum = nil);
var
  Z: TNum;
begin
  SetDebugStage('TestSum');
  CheckNum(X);
  CheckNum(Y);
  Z := TNum.Create(RandomPrec);
  if R = nil then R := Z;
  try
    CheckNum(Z);
    R.Sum(X,Y);
    CheckNum(Z);
    Z.Diff(R,Y);
    CheckNum(Z);
    CheckEquals(Z,X);
  finally
    Z.Free;
  end;
end;

procedure TestDiff(X,Y: TNum; R: TNum = nil);
var
  Z: TNum;
begin
  SetDebugStage('TestDiff');
  CheckNum(X);
  CheckNum(Y);
  Z := TNum.Create(RandomPrec);
  if R = nil then R := Z;
  try
    CheckNum(Z);
    R.Diff(X,Y);
    CheckNum(Z);
    Z.Sum(R,Y);
    CheckNum(Z);
    CheckEquals(Z,X);
  finally
    Z.Free;
  end;
end;

procedure TestProd(X,Y: TNum; R: TNum = nil);
var
  Z,R1: TNum;
begin
  SetDebugStage('TestProd');
  CheckNum(X);
  CheckNum(Y);
  Z := TNum.Create(X.DecPrec+2);
  if R = nil then R1 := Z else R1 := TNum.Create(R.DecPrec+2);
  try
    CheckNum(Z);
    R1.Prod(X,Y);
    CheckNum(Z);
    try
      Z.Quot(R1,Y);
    except
      on ENumInvalidArg do
        Z.Copy(X);
    end;
    CheckNum(Z);
    CheckEquals(Z,X);
    if R <> nil then R.Copy(R1);
    WriteLn(R1.AsString);
  finally
    if R <> nil then R1.Free;
    Z.Free;
  end;
end;

procedure TestQuot(X,Y: TNum; R: TNum = nil);
var
  Z,R1: TNum;
  YZero: Boolean;
begin
  SetDebugStage('TestQuot');
  CheckNum(X);
  //CheckNum(Y);
  Z := TNum.Create(X.DecPrec+2);
  if R = nil then R1 := Z else R1 := TNum.Create(R.DecPrec+2);
  try
    CheckNum(Z);
    YZero := Y.Sign = Zero;
    try
      R1.Quot(X,Y);
      if YZero then raise ENum.Create('DivByZero - no exception');
      CheckNum(Z);
      Z.Prod(R1,Y);
      CheckNum(Z);
    except
      on ENumDivByZero do
        if YZero then Z.Copy(X)
                 else raise;
      on ENumInvalidArg do
        if YZero then Z.Sign := Zero
                 else raise;
    end;
    CheckEquals(Z,X);
    if R <> nil then R.Copy(R1);
  finally
    if R <> nil then R1.Free;
    Z.Free;
  end;
end;

procedure TestSqr(X: TNum);
var
  Z: TNum;
  XNeg: Boolean;
begin
  SetDebugStage('TestSqr');
  CheckNum(X);
  Z := TNum.Create(RandomPrec);
  try
    CheckNum(Z);
    XNeg := X.Sign = Negative;
    Z.Prod(X,X);
    CheckNum(Z);
    Z.Sqrt_(Z);
    CheckNum(Z);
    if XNeg then Z.Sign := NegSign(Z.Sign);
    CheckEquals(Z,X);
  finally
    Z.Free;
  end;
end;

procedure TestSqrt(X: TNum);
var
  Z: TNum;
  XNeg: Boolean;
begin
  SetDebugStage('TestSqrt');
  CheckNum(X);
  Z := TNum.Create(RandomPrec);
  try
    CheckNum(Z);
    XNeg := X.Sign = Negative;
    try
      Z.Sqrt_(X);
      if XNeg then raise ENum.Create('Sqrt(negative) - no exception');
      CheckNum(Z);
      Z.Prod(Z,Z);
    except
      on ENumInvalidArg do
        if XNeg then Z.Copy(X)
                else raise;
    end;
    CheckNum(Z);
    CheckEquals(Z,X);
  finally
    Z.Free;
  end;
end;

procedure TestLn(X: TNum);
var
  Z: TNum;
  XPos: Boolean;
begin
  SetDebugStage('TestLn');
  CheckNum(X);
  Z := TNum.Create(RandomPrec);
  try
    CheckNum(Z);
    XPos := X.Sign = Positive;
    try
      Z.Ln_(X);
      if not XPos then raise ENum.Create('Ln(non-positive) - no exception');
      CheckNum(Z);
      Z.Exp_(Z);
    except
      on ENumInvalidArg do
        if XPos then raise
                else Z.Copy(X);
    end;
    CheckNum(Z);
    CheckEquals(Z,X);
  finally
    Z.Free;
  end;
end;

const
  Ln10 = 2.30258509299404568;
  LnMaxPlus = ( Ln10 + 1E-15) * (MaxDecExp+1);
  LnMinPlus = (-Ln10 + 1E-15) * (MaxDecExp+1);

procedure TestExp(X: TNum);
var
  Z: TNum;
begin
  SetDebugStage('TestExp');
  CheckNum(X);
  if X.GT(LnMaxPlus) or X.LT(LnMinPlus) then Exit;
  Z := TNum.Create(RandomPrec);
  try
    CheckNum(Z);
    Z.Exp_(X);
    CheckNum(Z);
    Z.Ln_(Z);
    CheckNum(Z);
    CheckEquals(Z,X);
  finally
    Z.Free;
  end;
end;

procedure TestTrig(X: TNum);
var
  Z, XScaled: TAddNum;
  i: Integer;
begin
  SetDebugStage('TestTrig');
  CheckNum(X);
  ArStacks.SetDeg(False);
  Z := TAddNum.Create(RandomPrec);
  XScaled := TAddNum.Create(X.DecPrec);
  try
    XScaled.Copy(X);
    for i := 0 to 10 do
    begin
      if not (XScaled.GT(+1) or XScaled.LT(-1))
      then Break;
      XScaled.Quot(XScaled, 10);
    end;
    if XScaled.GT(+1) or XScaled.LT(-1)
    then raise ENum.Create('Number is too big: ' + X.AsString);

    SetDebugStage('TestTrig.Sin');
    Z.DgArcSin(XScaled);
    CheckNum(Z);
    Z.Sin_(Z);
    CheckNum(Z);
    CheckEquals(Z,XScaled);

    SetDebugStage('TestTrig.Cos');
    Z.DgArcCos(XScaled);
    CheckNum(Z);
    Z.Cos_(Z);
    CheckNum(Z);
    CheckEquals(Z,XScaled);

    SetDebugStage('TestTrig.Tan');
    Z.DgArcTan(X);
    CheckNum(Z);
    Z.DgTan(Z);
    CheckNum(Z);
    CheckEquals(Z,X);
  finally
    Z.Free;
  end;
end;

procedure TestIntPower(X: TNum; Pow: Integer);
var
  m: Boolean;
  k,WorkPrec: Integer;
  Y: TNum;
begin
  SetDebugStage('TestIntPower');
  m := False;
  if Pow = 0 then
  begin
    //Copy(numOne);
    Exit;
  end;
  if Pow < 0 then
  begin
    Pow := -Pow;
    m := True
  end;
  WorkPrec := 100{DecPrec} + DecLen(Pow) + 1;
  {$RANGECHECKS OFF}
  Y := TNum.Create(WorkPrec);
  {$IFDEF ARKRNL_RANGE} {$RANGECHECKS ON} {$ENDIF}
  try
    Y.Copy(numOne);
    k := 1;
    if Pow >= $40000000 then k := $40000000
    else while k*2 <= Pow do k := k*2;
    try
      repeat
        TestProd(Y,Y,Y);
        if Pow >= k then
        begin
          Dec(Pow,k);
          TestProd(Y,X,Y);
        end;
        k := k shr 1
      until k = 0;
      {if m
      then Quot(numOne,Y)
      else Copy(Y);}
    except
      on ENumOverflow do
        if m then //Sign := Zero
             else raise;
    end;
  finally
    Y.Free;
  end;
end;

procedure TestValidateDecs(X: TNum);
var
  Y,Z: TNum;
  i,TempDecExp,DecExpInc: Integer;
begin
  SetDebugStage('TestValidateDecs');
  if X.Sign = Zero then
  begin
    Exit;
  end;
  Z := nil;
  Y := nil;
  try
    {$RANGECHECKS OFF}
    Z := TNum.Create(X.DecPrec+2);
    Y := TNum.Create(X.DecPrec+2);
    {$IFDEF ARKRNL_RANGE} {$RANGECHECKS ON} {$ENDIF}
    Z.Copy(X);
    Z.Sign := Positive;
    i := 0; TempDecExp := 1;
    while (i < Ten_Count) and Z.GE(Ten_P2_P[i]) do
    begin
      Inc(i);
      TempDecExp := TempDecExp shl 1;
    end;
    Dec(i);
    TempDecExp := TempDecExp shr 1;
    if i >= 0
    then begin
      TestQuot(Z,Ten_P2_P[i],Z);
      DecExpInc := TempDecExp shr 1;
      Dec(i);
      while i >= 0 do
      begin
        if Z.GE(Ten_P2_P[i]) then
        begin
          TestQuot(Z,Ten_P2_P[i],Z);
          Inc(TempDecExp,DecExpInc);
        end;
        Dec(i);
        DecExpInc := DecExpInc shr 1;
      end;
    end
    else if Z.GE(numOne)
         then TempDecExp := 0
         else begin
           TempDecExp := 1;
           i := 0;
           repeat
             Z.Prod(X,Ten_P2_P[i]);
             Z.Sign := Positive;
             Inc(i);
             TempDecExp := TempDecExp shl 1;
           until (i >= Ten_Count) or Z.GE(numOne);
           if Z.GE(numOne)
           then begin
             Dec(i);
             DecExpInc := TempDecExp shr 2;
           end
           else DecExpInc := TempDecExp shr 1;
           TempDecExp := 0;
           Dec(i);
           Z.Copy(X);
           Z.Sign := Positive;
           while i >= 0 do
           begin
             Y.Copy(Z);
             Z.Prod(Z,Ten_P2_P[i]);
             if Z.LT(numOne)
             then Inc(TempDecExp,DecExpInc)
             else Z.Copy(Y);
             Dec(i);
             DecExpInc := DecExpInc shr 1;
           end;
           Z.Prod(Z,numTen);
           TempDecExp := -TempDecExp-1;
         end;
    // 1 <= X < 10
    Z.Diff(Z,numOne);
    WriteLn(Z.AsString);
  finally
    Y.Free;
    Z.Free;
  end;
end;


initialization
  Randomize;
  WriteLn('RandSeed = ',RandSeed);
finalization
  //ReadLn;
end.
