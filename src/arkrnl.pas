{ 
  arith - arbitrary precision floaing point library in Delphi.
  kernel unit
}
unit ArKrnl;

interface

uses SysUtils, Classes, RTLConsts;

{$I DEFINES.INC}

const
  MinDecPrec = 3; { Минимальная точность в значащих десятичных цифрах }
  MaxDecPrec = 10000; { Максимальная точность в значащих десятичных цифрах }
  MaxDecExp =  99999999; { Максимальный показатель десятичной экспоненты }
  MaxDecExpLen = 8;

  Log_2_10 = 3.32192809488736235; { Log_2 10 }
  DecToQWord = Log_2_10 / 64; { Log_(2^64) 10 }
  SafeQWordCount = 3; { Количество дополнительных четверных слов для вычислений }
  MaxQWordPrec = Trunc(MaxDecPrec*DecToQWord + SafeQWordCount + 1);
                  { Максимальная точность в четверных словах }
  MaxBinExp = Trunc((MaxDecExp+1)*Log_2_10);
                  { Максимальный показатель двоичной экспоненты }

type
  TDecChar = '0'..'9';
  TDecDigit = 0..9;
  TDecDigits = array[0..MaxDecPrec-1] of TDecDigit;
  TDecChars = array[0..MaxDecPrec-1] of TDecChar;
  TDecPrec = MinDecPrec..MaxDecPrec;

  TTransformFlags = set of (tfXAbs, tfXNeg, tfYAbs, tfYNeg);

  DWord = LongInt;

  QWord = record
    Lo: DWord;
    Hi: DWord;
  end;

{$IFNDEF D4PLUS}
  Int64 = QWord;
{$ENDIF}

  TDWordDigits = array[0..2*MaxQWordPrec-1] of DWord;
  TQWordDigits = array[0..MaxQWordPrec-1] of QWord;
  TQWordPrec = 1..MaxQWordPrec;
  TDecExp = -MaxDecExp..MaxDecExp;
  TBinExp = Integer;
  TSign = (Zero,Positive,Negative);

  PDWord = ^DWord;
  PQWord = ^QWord;
  PDWordDigits = ^TDWordDigits;
  PQWordDigits = ^TQWordDigits;
  PDecDigits = ^TDecDigits;
  PDecChars = ^TDecChars;

const
  MinDWord = DWord($80000000);
  MinQWord: QWord = (Lo: 0; Hi: MinDWord);

{$IFDEF D4PLUS}
  MinInt64 = Int64($10000)*$10000 shl 31; { $8000000000000000 }
{$ELSE}
  MinInt64: QWord = (Lo: 0; Hi: MinDWord);
{$ENDIF}

  // Коды исключений
  ENUM_UNKNOWN = -1;
  ENUM_CONVERT = -2;
  ENUM_DIV_BY_ZERO = -3;
  ENUM_INVALID_ARG = -4;
  ENUM_OVERFLOW = -5;
  ENUM_VALUE = -6;
  ENUM_CONST = -7;

type
  ENum = class(Exception)
  protected
    function GetErrorCode: Integer; virtual;
  public
    property ErrorCode: Integer read GetErrorCode;
  end;

  ENumConvertError = class(ENum)
  protected
    function GetErrorCode: Integer; override;
  end;

  ENumDivByZero = class(ENum)
  protected
    function GetErrorCode: Integer; override;
  end;

  ENumInvalidArg = class(ENum)
  protected
    function GetErrorCode: Integer; override;
  end;

  ENumOverflow = class(ENum)
  protected
    function GetErrorCode: Integer; override;
  end;

  ENumValError = class(ENum)
  private
    FErrorPos: PChar;
  protected
    function GetErrorCode: Integer; override;
  public
    constructor CreatePos(AnErrorPos: PChar);
    constructor CreatePosMsg(AnErrorPos: PChar; const Msg: string);
    property ErrorPos: PChar read FErrorPos write FErrorPos;
  end;

  ENumConst = class(ENum)
  protected
    function GetErrorCode: Integer; override;
  end;

  TNum = class(TPersistent)
  private
    FQWordPrec: TQWordPrec;
    FQWordDigits: PQWordDigits;
    FBinExp: Integer;
    FSign: TSign;

    FDecPrec: TDecPrec;
    FDecDigits: PDecDigits;
    FDecExp: TDecExp;

    FDecsValid: Boolean;
    FQWordsValid: Boolean;
    FIsConst: Boolean;

    procedure CheckMinMax;

    function  GetAsString: string;
    function  GetAsInteger: Integer;
    function  GetAsInt64: Int64;
    function  GetAsFloat: Extended;
    function  GetAsComp: Comp;

    procedure SetAsString(Value: string);
    procedure SetAsInteger(Value: Integer);
    procedure SetAsInt64(Value: Int64);
    procedure SetAsFloat(Value: Extended);
    procedure SetAsComp(Value: Comp);

    function  GetIsInteger: Boolean;
    function  GetIsReal: Boolean;
    function  GetIsSingle: Boolean;
    function  GetIsDouble: Boolean;
    function  GetIsExtended: Boolean;
    function  GetIsInt64: Boolean;

    procedure DoValidateDecs;
    procedure DoValidateQWords;

    function  GetDecDigit(Index: Integer): TDecDigit;
    function  GetDecChar(Index: Integer): TDecChar;
    procedure SetDecDigit(Index: Integer; Digit: TDecDigit);
    procedure SetDecChar(Index: Integer; C: TDecChar);

    procedure SetDecPrec(ADecPrec: TDecPrec);
    function  GetBinExp: Integer;
    procedure SetBinExp(ABinExp: TBinExp);
    function  GetDecExp: TDecExp;
    procedure SetDecExp(ADecExp: TDecExp);
    procedure SetSign(ASign: TSign);

    function  GetQWordDigit(Index: Integer): QWord;
    procedure SetQWordDigit(Index: Integer; const Digit: QWord);

    function  GetNonZeroDigits: Integer;
    function  GetNonZeroBits: Integer;
  public
    constructor Create(ADecPrec: TDecPrec);
    constructor CreateConst(ADecPrec: TDecPrec; ASign: TSign;
                            ABinExp: Integer; AQWordDigits: PQWordDigits);
    destructor  Destroy; override;
    procedure   ValidateDecs;
    procedure   ValidateQWords;
    procedure   Copy(X: TNum);
    procedure   Assign(Source: TPersistent); override;
    procedure   AssignTo(Dest: TPersistent); override;
    procedure   CheckZero(ABinExp,ADecPrec: Integer);

    function    GT(X: TNum): Boolean;
    {$IFDEF D4PLUS}
      overload;
    function    GT(X: Extended): Boolean; overload;
    function    GT(const X: string): Boolean; overload;
    {$ENDIF}
    function    GTE(X: Extended): Boolean;
    function    GTS(const X: string): Boolean;

    function    LT(X: TNum): Boolean;
    {$IFDEF D4PLUS}
      overload;
    function    LT(X: Extended): Boolean; overload;
    function    LT(const X: string): Boolean; overload;
    {$ENDIF}
    function    LTE(X: Extended): Boolean;
    function    LTS(const X: string): Boolean;

    function    GE(X: TNum): Boolean;
    {$IFDEF D4PLUS}
      overload;
    function    GE(X: Extended): Boolean; overload;
    function    GE(const X: string): Boolean; overload;
    {$ENDIF}
    function    GEE(X: Extended): Boolean;
    function    GES(const X: string): Boolean;

    function    LE(X: TNum): Boolean;
    {$IFDEF D4PLUS}
      overload;
    function    LE(X: Extended): Boolean; overload;
    function    LE(const X: string): Boolean; overload;
    {$ENDIF}
    function    LEE(X: Extended): Boolean;
    function    LES(const X: string): Boolean;

    function    Eq(X: TNum): Boolean;
    {$IFDEF D4PLUS}
      overload;
    function    Eq(X: Extended): Boolean; overload;
    function    Eq(const X: string): Boolean; overload;
    {$ENDIF}
    function    EqE(X: Extended): Boolean;
    function    EqS(const X: string): Boolean;

    procedure   Sum(X,Y: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Sum(X: TNum; Y: Extended); overload;
    procedure   Sum(X: TNum; const Y: string); overload;
    procedure   Sum(X,Y: Extended); overload;
    procedure   Sum(X: Extended; const Y: string); overload;
    procedure   Sum(const X,Y: string); overload;
    {$ENDIF}
    procedure   SumE(X: TNum; Y: Extended);
    procedure   SumS(X: TNum; const Y: string);
    procedure   SumEE(X,Y: Extended);
    procedure   SumES(X: Extended; const Y: string);
    procedure   SumSS(const X,Y: string);

    procedure   Prod(X,Y: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Prod(X: TNum; Y: Extended); overload;
    procedure   Prod(X: TNum; const Y: string); overload;
    procedure   Prod(X,Y: Extended); overload;
    procedure   Prod(X: Extended; const Y: string); overload;
    procedure   Prod(const X,Y: string); overload;
    {$ENDIF}
    procedure   ProdE(X: TNum; Y: Extended);
    procedure   ProdS(X: TNum; const Y: string);
    procedure   ProdEE(X,Y: Extended);
    procedure   ProdES(X: Extended; const Y: string);
    procedure   ProdSS(const X,Y: string);

    procedure   Diff(X,Y: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Diff(X: TNum; Y: Extended); overload;
    procedure   Diff(X: TNum; const Y: string); overload;
    procedure   Diff(X: Extended; Y: TNum); overload;
    procedure   Diff(const X: string; Y: TNum); overload;
    procedure   Diff(X,Y: Extended); overload;
    procedure   Diff(X: Extended; const Y: string); overload;
    procedure   Diff(const X: string; Y: Extended); overload;
    procedure   Diff(const X,Y: string); overload;
    {$ENDIF}
    procedure   DiffE(X: TNum; Y: Extended);
    procedure   DiffS(X: TNum; const Y: string);
    procedure   DiffER(X: Extended; Y: TNum);
    procedure   DiffSR(const X: string; Y: TNum);
    procedure   DiffEE(X,Y: Extended);
    procedure   DiffES(X: Extended; const Y: string);
    procedure   DiffSE(const X: string; Y: Extended);
    procedure   DiffSS(const X,Y: string);

    procedure   Quot(X,Y: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Quot(X: TNum; Y: Extended); overload;
    procedure   Quot(X: TNum; const Y: string); overload;
    procedure   Quot(X: Extended; Y: TNum); overload;
    procedure   Quot(const X: string; Y: TNum); overload;
    procedure   Quot(X,Y: Extended); overload;
    procedure   Quot(X: Extended; const Y: string); overload;
    procedure   Quot(const X: string; Y: Extended); overload;
    procedure   Quot(const X,Y: string); overload;
    {$ENDIF}
    procedure   QuotE(X: TNum; Y: Extended);
    procedure   QuotS(X: TNum; const Y: string);
    procedure   QuotER(X: Extended; Y: TNum);
    procedure   QuotSR(const X: string; Y: TNum);
    procedure   QuotEE(X,Y: Extended);
    procedure   QuotES(X: Extended; const Y: string);
    procedure   QuotSE(const X: string; Y: Extended);
    procedure   QuotSS(const X,Y: string);

    procedure   Neg(X: TNum);
    procedure   IntPower(X: TNum; Pow: Integer);
    procedure   Power(X,Y: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Power(X: TNum; Y: Extended); overload;
    procedure   Power(X: TNum; const Y: string); overload;
    procedure   Power(X: Extended; Y: TNum); overload;
    procedure   Power(const X: string; Y: TNum); overload;
    procedure   Power(X,Y: Extended); overload;
    procedure   Power(X: Extended; const Y: string); overload;
    procedure   Power(const X: string; Y: Extended); overload;
    procedure   Power(const X,Y: string); overload;
    {$ENDIF}
    procedure   PowerE(X: TNum; Y: Extended);
    procedure   PowerS(X: TNum; const Y: string);
    procedure   PowerER(X: Extended; Y: TNum);
    procedure   PowerSR(const X: string; Y: TNum);
    procedure   PowerEE(X,Y: Extended);
    procedure   PowerES(X: Extended; const Y: string);
    procedure   PowerSE(const X: string; Y: Extended);
    procedure   PowerSS(const X,Y: string);

    procedure   DivMod(X,Y,Remainder: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   DivMod(X: TNum; Y: Extended; Remainder: TNum = nil); overload;
    procedure   DivMod(X: TNum; const Y: string; Remainder: TNum = nil); overload;
    procedure   DivMod(X: Extended; Y: TNum; Remainder: TNum = nil); overload;
    procedure   DivMod(const X: string; Y: TNum; Remainder: TNum = nil); overload;
    procedure   DivMod(X,Y: Extended; Remainder: TNum = nil); overload;
    procedure   DivMod(X: Extended; const Y: string; Remainder: TNum = nil); overload;
    procedure   DivMod(const X: string; Y: Extended; Remainder: TNum = nil); overload;
    procedure   DivMod(const X,Y: string; Remainder: TNum = nil); overload;
    {$ENDIF}
    procedure   DivModE(X: TNum; Y: Extended; Remainder: TNum);
    procedure   DivModS(X: TNum; const Y: string; Remainder: TNum);
    procedure   DivModER(X: Extended; Y,Remainder: TNum);
    procedure   DivModSR(const X: string; Y,Remainder: TNum);
    procedure   DivModEE(X,Y: Extended; Remainder: TNum);
    procedure   DivModES(X: Extended; const Y: string; Remainder: TNum);
    procedure   DivModSE(const X: string; Y: Extended; Remainder: TNum);
    procedure   DivModSS(const X,Y: string; Remainder: TNum);

    procedure   Sin_(X: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Sin_(X: Extended); overload;
    procedure   Sin_(const X: string); overload;
    {$ENDIF}
    procedure   Sin_E(X: Extended);
    procedure   Sin_S(const X: string);

    procedure   Cos_(X: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Cos_(X: Extended); overload;
    procedure   Cos_(const X: string); overload;
    {$ENDIF}
    procedure   Cos_E(X: Extended);
    procedure   Cos_S(const X: string);

    procedure   ArcTan_(X: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   ArcTan_(X: Extended); overload;
    procedure   ArcTan_(const X: string); overload;
    {$ENDIF}
    procedure   ArcTan_E(X: Extended);
    procedure   ArcTan_S(const X: string);

    procedure   Exp_(X: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Exp_(X: Extended); overload;
    procedure   Exp_(const X: string); overload;
    {$ENDIF}
    procedure   Exp_E(X: Extended);
    procedure   Exp_S(const X: string);

    procedure   Ln_(X: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Ln_(X: Extended); overload;
    procedure   Ln_(const X: string); overload;
    {$ENDIF}
    procedure   Ln_E(X: Extended);
    procedure   Ln_S(const X: string);

    procedure   Abs_(X: TNum);
    procedure   Sqrt_(X: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Sqrt_(X: Extended); overload;
    procedure   Sqrt_(const X: string); overload;
    {$ENDIF}
    procedure   Sqrt_E(X: Extended);
    procedure   Sqrt_S(const X: string);

    procedure   Int_(X: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Int_(X: Extended); overload;
    {$ENDIF}
    procedure   Int_E(X: Extended);

    procedure   Frac_(X: TNum);
    procedure   Trunc_(X: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Trunc_(X: Extended); overload;
    {$ENDIF}
    procedure   Trunc_E(X: Extended);

    procedure   Round_(X: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Round_(X: Extended); overload;
    {$ENDIF}
    procedure   Round_E(X: Extended);

    procedure   Ceil_(X: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Ceil_(X: Extended); overload;
    {$ENDIF}
    procedure   Ceil_E(X: Extended);

    procedure   Floor_(X: TNum);
    {$IFDEF D4PLUS}
      overload;
    procedure   Floor_(X: Extended); overload;
    {$ENDIF}
    procedure   Floor_E(X: Extended);

    function    Odd_: Boolean;
    function    IntTrunc: Integer;
    function    IntRound: Integer;
    function    IntCeil: Integer;
    function    IntFloor: Integer;

    function    Int64Trunc: Int64;
    function    Int64Round: Int64;
    function    Int64Ceil: Int64;
    function    Int64Floor: Int64;

    procedure   Val_(var P: PChar; NumEnd0: Boolean);
    procedure   Str_(Size,APrec,Flags: Word; P: PChar);

    { Number properties }
    property DecPrec: TDecPrec read FDecPrec write SetDecPrec;
    property DecDigits[Index: Integer]: TDecDigit read GetDecDigit write SetDecDigit;
    property DecChars[Index: Integer]: TDecChar read GetDecChar write SetDecChar;
    property DecExp: TDecExp read GetDecExp write SetDecExp;
    property Sign: TSign read FSign write SetSign;
    property QWordPrec: TQWordPrec read FQWordPrec;
    property QWordDigits[Index: Integer]: QWord read GetQWordDigit write SetQWordDigit;
    property BinExp: Integer read GetBinExp write SetBinExp;
    property NonZeroDigits: Integer read GetNonZeroDigits;
    property NonZeroBits: Integer read GetNonZeroBits;

    property AsString: string read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsComp: Comp read GetAsComp write SetAsComp;

    property IsInteger: Boolean read GetIsInteger;
    property IsInt64: Boolean read GetIsInt64;
    property IsReal: Boolean read GetIsReal;
    property IsSingle: Boolean read GetIsSingle;
    property IsDouble: Boolean read GetIsDouble;
    property IsExtended: Boolean read GetIsExtended;
    property IsComp: Boolean read GetIsInt64;

    property IsConst: Boolean read FIsConst;
  end;

const
  ecnNormal   = [];
  ecnReverse  = [tfYNeg];
  ecnAbsolute = [tfXAbs, tfYAbs];

var
  numZero, numOne, numMinusOne, numTwo, numHalf, numTen, numPi: TNum;

function IntSign(S: TSign): Integer;
function SignOfInt(I: Integer): TSign;
function SignToStr(S: TSign): string;
function NegSign(S: TSign): TSign;
function AbsSign(S: TSign): TSign;
function MulSigns(S1,S2: TSign): TSign;
function CompareSigns(S1,S2: TSign): Integer;
function ExtCompareSigns(S1,S2: TSign; Flags: TTransformFlags): Integer;

function CompareNums(X,Y: TNum): Integer;
function RevCompareNums(X,MinusY: TNum): Integer;
function AbsCompareNums(X,Y: TNum): Integer;
function ExtCompareNums(X,Y: TNum; Flags: TTransformFlags;
           YShift: Integer): Integer;

procedure CreateNumDefs(Dir, Name: string; Nums: TStrings);
function DecLen(N: Integer): Integer;
function IntMin(X,Y: LongInt): LongInt;
function IntMax(X,Y: LongInt): LongInt;

implementation

uses Consts, Controls {for Assign, AssignTo}, Const_00, NumMsgs;

const
  Ln10 = 2.30258509299404568;
  LnMaxPlus = ( Ln10 + 1E-15) * (MaxDecExp+1);
  LnMinPlus = (-Ln10 + 1E-15) * (MaxDecExp+1);

type
  TCommonBuffer = array[0..MaxQWordPrec+1] of QWord;

  TExtendedRec = packed record
    Mantissa: QWord;
    SignExp: SmallInt;
  end;

function IntMin(X,Y: LongInt): LongInt;
begin
  if X > Y then Result := Y else Result := X;
end;

function IntMax(X,Y: LongInt): LongInt;
begin
  if X < Y then Result := Y else Result := X;
end;

function IntSign(S: TSign): Integer;
begin
  case S of
    Negative: Result := -1;
    Positive: Result := +1;
    else Result := 0;
  end;
end;

function SignOfInt(I: Integer): TSign;
begin
  if I = 0
  then Result := Zero
  else if I > 0
       then Result := Positive
       else Result := Negative;
end;

function SignToStr(S: TSign): string;
begin
  case S of
    Zero:     Result := 'Zero';
    Negative: Result := 'Negative';
    Positive: Result := 'Positive';
    else      Result := 'erroneous sign';
  end;
end;

function NegSign(S: TSign): TSign;
begin
  case S of
    Negative: Result := Positive;
    Positive: Result := Negative;
    else Result := S;
  end;
end;

function AbsSign(S: TSign): TSign;
begin
  if S = Zero
  then Result := Zero
  else Result := Positive;
end;

function MulSigns(S1,S2: TSign): TSign;
begin
  case S1 of
    Negative: Result := NegSign(S2);
    Positive: Result := S2;
    else Result := Zero;
  end;
end;

function CompareSigns(S1,S2: TSign): Integer;
begin
  Result := IntSign(S1) - IntSign(S2);
end;

procedure TransformSigns(var S1,S2: TSign; Flags: TTransformFlags);
begin
  if tfXAbs in Flags then S1 := AbsSign(S1);
  if tfXNeg in Flags then S1 := NegSign(S1);
  if tfYAbs in Flags then S2 := AbsSign(S2);
  if tfYNeg in Flags then S2 := NegSign(S2);
end;

function ExtCompareSigns(S1,S2: TSign; Flags: TTransformFlags): Integer;
begin
  TransformSigns(S1,S2,Flags);
  Result := CompareSigns(S1,S2);
end;

{$I LowLevel.INC}

constructor TNum.Create(ADecPrec: TDecPrec);
begin
  inherited Create;
  FDecPrec := ADecPrec;
  FQWordPrec := DecsToQWords(ADecPrec);
  FQWordDigits := AllocMem(FQWordPrec*8);
  FBinExp := 0;
  FDecExp := 0;
  FSign := Zero;
  FDecsValid := False;
  FQWordsValid := True;
end;

constructor TNum.CreateConst(ADecPrec: TDecPrec; ASign: TSign;
                        ABinExp: Integer; AQWordDigits: PQWordDigits);
begin
  inherited Create;
  FDecPrec := ADecPrec;
  FQWordPrec := DecsToQWords(ADecPrec);
  FQWordDigits := AQWordDigits;
  FSign := ASign;
  FBinExp := ABinExp;
  FDecsValid := False;
  FQWordsValid := True;
  FIsConst := True;
end;

destructor TNum.Destroy;
begin
  if not IsConst then ReallocMem(FQWordDigits,0);
  ReallocMem(FDecDigits,0);
  inherited Destroy;
end;

procedure TNum.Copy(X: TNum);
var
  Shorter: Boolean;
begin
  if X = Self then Exit;
  if IsConst then raise ENumConst.Create(SNumConst);
  X.ValidateQWords;
  FSign := X.FSign;
  FBinExp := X.FBinExp;
  Shorter := FQWordPrec < X.FQWordPrec;
  if Shorter
  then begin
    CopyQWords(FQWordDigits,@X.FQWordDigits[X.FQWordPrec-FQWordPrec],FQWordPrec);
    if X.FQWordDigits[X.FQWordPrec-FQWordPrec-1].Hi < 0 then
    begin
      if IncQWords(FQWordDigits,FQWordPrec) <> 0
      then begin
        if X.FBinExp >= MaxBinExp
        then begin
          raise ENumOverflow.Create(SOverflow);
        end
        else begin
          FBinExp := X.FBinExp + 1;
          FQWordDigits[FQWordPrec-1].Hi := MinDWord;
        end;
      end;
    end;
  end
  else begin
    ZeroQWords( FQWordDigits,FQWordPrec-X.FQWordPrec);
    CopyQWords(@FQWordDigits[FQWordPrec-X.FQWordPrec],X.FQWordDigits,X.FQWordPrec);
  end;
  FQWordsValid := True;
  FDecsValid := False;
end;

type
TAuxControl = class(TControl)
public
  property Caption; { Нужно опубликовать эти свойства, чтобы пользоваться }
  property Text;
end;

procedure TNum.Assign(Source: TPersistent);
begin
  if Source is TNum
  then Copy(TNum(Source))
  else if Source is TControl
       then if TAuxControl(Source).Text = TComponent(Source).Name
            then AsString := TAuxControl(Source).Caption
            else AsString := TAuxControl(Source).Text
end;

procedure TNum.AssignTo(Dest: TPersistent);
begin
  if Dest is TNum
  then TNum(Dest).Copy(Self)
  else if Dest is TControl
       then if TAuxControl(Dest).Text = TComponent(Dest).Name
            then TAuxControl(Dest).Caption := AsString
            else TAuxControl(Dest).Text := AsString;
end;

function ExtCompareNums(X,Y: TNum; Flags: TTransformFlags;
           YShift: Integer): Integer;
{ Расширенное сравнение чисел. Перед сравнением:
    1) если tfXAbs in Flags, X берётся по абсолютной величине;
       если tfXNeg in Flags, X изменяет знак;
       флаги tfYAbs, tfYNeg аналогично влияют на Y;
    2) Y умножается на 2^YShift
}
begin
  X.ValidateQWords;
  Y.ValidateQWords;
  Result := ExtCompareSigns(X.FSign,Y.FSign,Flags);
  if (Result <> 0) or (X.FSign = Zero) then Exit;
  Result := X.FBinExp - (Y.FBinExp + YShift);
  if Result = 0
  then Result := CmpQWords(X.FQWordDigits,Y.FQWordDigits,
                           X.FQWordPrec,  Y.FQWordPrec);
  if   (not (tfXAbs in Flags))
   and (X.FSign = Negative)
  then Result := -Result;
  if tfXNeg in Flags
  then Result := -Result;
end;

function CompareNums(X,Y: TNum): Integer;
begin
  Result := ExtCompareNums(X,Y,ecnNormal,0);
end;

function RevCompareNums(X,MinusY: TNum): Integer;
begin
  Result := ExtCompareNums(X,MinusY,ecnReverse,0);
end;

function AbsCompareNums(X,Y: TNum): Integer;
begin
  Result := ExtCompareNums(X,Y,ecnAbsolute,0);
end;

function TNum.GT(X: TNum): Boolean;
begin
  Result := CompareNums(Self,X) > 0;
end;

function TNum.LT(X: TNum): Boolean;
begin
  Result := CompareNums(Self,X) < 0;
end;

function TNum.GE(X: TNum): Boolean;
begin
  Result := CompareNums(Self,X) >= 0;
end;

function TNum.LE(X: TNum): Boolean;
begin
  Result := CompareNums(Self,X) <= 0;
end;

function  TNum.Eq(X: TNum): Boolean;
begin
  Result := CompareNums(Self,X) = 0;
end;


procedure SwapNums(var X,Y: TNum);
var
  Tmp: TNum;
begin
  Tmp := X;
  X := Y;
  Y := Tmp;
end;

function ActualPrec(X,Y: TNum): Integer;
begin
  Result := 0;
  if X = nil then SwapNums(X,Y);
  if X = nil then Exit;
  if Y = nil
  then Result := X.DecPrec
  else if X.IsConst or Y.IsConst
       then Result := IntMax(X.DecPrec,Y.DecPrec)
       else Result := IntMin(X.DecPrec,Y.DecPrec);
end;


procedure TNum.CheckMinMax;
{ Проверяет, не вышло ли число за пределы диапазона представления }
var
  SaveSign: TSign;
begin
  if AbsCompareNums(Self,MaxNum) > 0
  then raise ENumOverflow.Create(SOverflow);
  if (Sign <> Zero) and (AbsCompareNums(Self,MinNum) < 0)
  then if (ExtCompareNums(Self,MinNum,ecnAbsolute,-1) < 0)
       then Sign := Zero
       else begin
         SaveSign := FSign;
         Copy(MinNum);
         FSign := SaveSign;
       end;
end;

procedure TNum.CheckZero(ABinExp,ADecPrec: Integer);
{ Если 2^ABinExp / Self > 10^ADecPrec (грубо!), то Self обнуляется
  Полезная процедура для нек-рых операций (вычитание, синус, и т.д.)
  Если результат операции близок к нулю по сравнению с аргументом(-ами)
  (критерий близости - точность аргумента(-ов)), то этот результат
  считается нулём. }
begin
  if   (Sign <> Zero)
   and (Trunc((ABinExp-BinExp-1)/Log_2_10) > ADecPrec)
  then Sign := Zero;
end;

procedure TNum.Sum(X,Y: TNum);
var
  TempExp,CheckExp,CheckPrec: Integer;
  TempSign: TSign;
  Buffer: TCommonBuffer;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  X.ValidateQWords;
  Y.ValidateQWords;
  CheckExp := IntMin(X.BinExp,Y.BinExp);
  CheckPrec := ActualPrec(X,Y);
  if X.FSign = Zero
  then begin
    Copy(Y);
    Exit;
  end;
  if Y.FSign = Zero
  then begin
    Copy(X);
    Exit;
  end;
  TempSign := Positive;
  if X.FSign = Y.FSign
  then TempExp :=  SumOfQWords(X.FQWordDigits,Y.FQWordDigits,
                     @Buffer,X.FQWordPrec,Y.FQWordPrec,FQWordPrec,
                     X.FBinExp,Y.FBinExp)
  else TempExp := DiffOfQWords(X.FQWordDigits,Y.FQWordDigits,
                     @Buffer,X.FQWordPrec,Y.FQWordPrec,FQWordPrec,
                     X.FBinExp,Y.FBinExp,TempSign);
  if TempExp > MaxBinExp then raise ENumOverflow.Create(SOverflow);
  if TempExp < -MaxBinExp then
  begin
    Sign := Zero;
    Exit;
  end;
  CopyQWords(FQWordDigits,@Buffer,FQWordPrec);
  FBinExp := TempExp;
  case TempSign of
    Zero: FSign := Zero;
    Negative: FSign := Y.FSign;
    Positive: FSign := X.FSign;
  end;
  CheckMinMax;
  CheckZero(CheckExp,CheckPrec);
  FDecsValid := False;
end;

procedure TNum.Diff(X,Y: TNum);
var
  TempExp,CheckExp,CheckPrec: Integer;
  TempSign: TSign;
  Buffer: TCommonBuffer;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  X.ValidateQWords;
  Y.ValidateQWords;
  CheckExp := IntMin(X.BinExp,Y.BinExp);
  CheckPrec := ActualPrec(X,Y);
  if X.FSign = Zero
  then begin
    Copy(Y);
    FSign := NegSign(FSign);
    Exit;
  end;
  if Y.FSign = Zero
  then begin
    Copy(X);
    Exit;
  end;
  TempSign := Positive;
  if X.FSign = Y.FSign
  then TempExp := DiffOfQWords(X.FQWordDigits,Y.FQWordDigits,
                     @Buffer,X.FQWordPrec,Y.FQWordPrec,FQWordPrec,
                     X.FBinExp,Y.FBinExp,TempSign)
  else TempExp :=  SumOfQWords(X.FQWordDigits,Y.FQWordDigits,
                     @Buffer,X.FQWordPrec,Y.FQWordPrec,FQWordPrec,
                     X.FBinExp,Y.FBinExp);
  if TempExp > MaxBinExp then raise ENumOverflow.Create(SOverflow);
  if TempExp < -MaxBinExp then
  begin
    Sign := Zero;
    Exit;
  end;
  FBinExp := TempExp;
  CopyQWords(FQWordDigits,@Buffer,FQWordPrec);
  FSign := MulSigns(X.FSign,TempSign);
  CheckMinMax;
  CheckZero(CheckExp,CheckPrec);
  FDecsValid := False;
end;

procedure TNum.Prod(X,Y: TNum);
var
  TempExp: Integer;
  Buffer: TCommonBuffer;
  CalcPrec: Integer;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  X.ValidateQWords;
  Y.ValidateQWords;
  if (X.FSign = Zero) or (Y.FSign = Zero) then
  begin
    Sign := Zero;
    Exit;
  end;
  TempExp := X.FBinExp + Y.FBinExp;
  if TempExp < -MaxBinExp-2 then
  begin
    Sign := Zero;
    Exit;
  end;
  CalcPrec := FQWordPrec+2;
  MulQWords(X.FQWordDigits,Y.FQWordDigits,@Buffer,
            X.FQWordPrec,  Y.FQWordPrec,CalcPrec);
  Inc(TempExp,1-NormalizeQWords(@Buffer,CalcPrec));
  if Buffer[1].Hi < 0
  then if IncQWords(@Buffer[2],FQWordPrec) <> 0 then
       begin
         Inc(TempExp);
         Buffer[FQWordPrec+1].Hi := MinDWord;
       end;
  if TempExp > MaxBinExp then raise ENumOverflow.Create(SOverflow);
  if TempExp < -MaxBinExp then
  begin
    Sign := Zero;
    Exit;
  end;
  FBinExp := TempExp;
  CopyQWords(FQWordDigits,@Buffer[2],FQWordPrec);
  FSign := MulSigns(X.FSign,Y.FSign);
  CheckMinMax;
  FDecsValid := False;
end;

type
TDivBuffer = record
  LeadingDWord: DWord;
  Buffer: TCommonBuffer;
  TrailingDWord: DWord;
end;

procedure TNum.Quot(X,Y: TNum);
var
  TempExp: Integer;
  Dvd, Dvs: TDivBuffer;
  Buffer: TCommonBuffer;
  CalcPrec: Integer;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  X.ValidateQWords;
  Y.ValidateQWords;
  if X.FSign = Zero then
  begin
    if Y.FSign = Zero then raise ENumInvalidArg.Create(SInvalidArg);
    Sign := Zero;
    Exit;
  end;
  if Y.FSign = Zero then raise ENumDivByZero.Create(SDivByZero);
  TempExp := X.FBinExp - Y.FBinExp;
  if TempExp < -MaxBinExp then
  begin
    Sign := Zero;
    Exit;
  end;
  CalcPrec := FQWordPrec+2;
  Dvd.LeadingDWord := 0;
  Dvs.LeadingDWord := 0;
  if X.FQWordPrec < CalcPrec
  then begin
    CopyQWords(@Dvd.Buffer[CalcPrec-X.FQWordPrec],X.FQWordDigits,X.FQWordPrec);
    ZeroQWords(@Dvd.Buffer,CalcPrec-X.FQWordPrec);
  end
  else CopyQWords(@Dvd.Buffer,@X.FQWordDigits[X.FQWordPrec-CalcPrec],CalcPrec);
  if Y.FQWordPrec < CalcPrec
  then begin
    CopyQWords(@Dvs.Buffer[CalcPrec-Y.FQWordPrec],Y.FQWordDigits,Y.FQWordPrec);
    ZeroQWords(@Dvs.Buffer,CalcPrec-Y.FQWordPrec);
  end
  else CopyQWords(@Dvs.Buffer,@Y.FQWordDigits[Y.FQWordPrec-CalcPrec],CalcPrec);
  if CalcPrec > High(TCommonBuffer)
  then begin
    Dvd.TrailingDWord := 0;
    Dvs.TrailingDWord := 0;
  end
  else begin
    Dvd.Buffer[CalcPrec].Lo := 0;
    Dvs.Buffer[CalcPrec].Lo := 0;
  end;
  DivQWords(@Dvd.Buffer,@Dvs.Buffer,@Buffer,CalcPrec);
  Dec(TempExp,NormalizeQWords(@Buffer,CalcPrec)-63);
  if Buffer[1].Hi < 0
  then if IncQWords(@Buffer[2],FQWordPrec) <> 0 then
       begin
         Inc(TempExp);
         Buffer[FQWordPrec+1].Hi := MinDWord;
       end;
  if TempExp > MaxBinExp then raise ENumOverflow.Create(SOverflow);
  if TempExp < -MaxBinExp then
  begin
    Sign := Zero;
    Exit;
  end;
  FBinExp := TempExp;
  CopyQWords(FQWordDigits,@Buffer[2],FQWordPrec);
  FSign := MulSigns(X.FSign,Y.FSign);
  CheckMinMax;
  FDecsValid := False;
end;

function DecimalDigit(DW: DWord; BinExp: Integer): TDecDigit;
begin
  if BinExp < 0
  then Result := 0
  else Result := DW shr (31-BinExp);
end;

procedure TNum.DoValidateDecs;
var
  X,Y: TNum;
  i,TempDecExp,DecExpInc: Integer;
  NextDigit: TDecDigit;
  Carry: Boolean;
  Buffer: TDecDigits;
begin
  CheckMinMax;
  ReallocMem(FDecDigits,FDecPrec);
  if FSign = Zero then
  begin
    FillChar(FDecDigits^,FDecPrec,0);
    FDecExp := 0;
    FDecsValid := True;
    Exit;
  end;
  X := nil;
  Y := nil;
  try
    {$R-}
    X := TNum.Create(FDecPrec+2);
    Y := TNum.Create(FDecPrec+2);
    {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
    X.Copy(Self);
    X.FSign := Positive;
    i := 0; TempDecExp := 1;
    while (i < Ten_Count) and X.GE(Ten_P2_P[i]) do
    begin
      Inc(i);
      TempDecExp := TempDecExp shl 1;
    end;
    Dec(i);
    TempDecExp := TempDecExp shr 1;
    if i >= 0
    then begin
      X.Quot(X,Ten_P2_P[i]);
      DecExpInc := TempDecExp shr 1;
      Dec(i);
      while i >= 0 do
      begin
        if X.GE(Ten_P2_P[i]) then
        begin
          X.Quot(X,Ten_P2_P[i]);
          Inc(TempDecExp,DecExpInc);
        end;
        Dec(i);
        DecExpInc := DecExpInc shr 1;
      end;
    end
    else if X.GE(numOne)
         then TempDecExp := 0
         else begin
           TempDecExp := 1;
           i := 0;
           repeat
             X.Prod(Self,Ten_P2_P[i]);
             X.FSign := Positive;
             Inc(i);
             TempDecExp := TempDecExp shl 1;
           until (i >= Ten_Count) or X.GE(numOne);
           if X.GE(numOne)
           then begin
             Dec(i);
             DecExpInc := TempDecExp shr 2;
           end
           else DecExpInc := TempDecExp shr 1;
           TempDecExp := 0;
           Dec(i);
           X.Copy(Self);
           X.FSign := Positive;
           while i >= 0 do
           begin
             Y.Copy(X);
             X.Prod(X,Ten_P2_P[i]);
             if X.LT(numOne)
             then Inc(TempDecExp,DecExpInc)
             else X.Copy(Y);
             Dec(i);
             DecExpInc := DecExpInc shr 1;
           end;
           X.Prod(X,numTen);
           TempDecExp := -TempDecExp-1;
         end;
    { 1 <= X < 10 }
    Y.Copy(numOne);
    for i := 0 to FDecPrec-1 do
    begin
      NextDigit := DecimalDigit(X.FQWordDigits[X.FQWordPrec-1].Hi,X.FBinExp);
      Buffer[i] := NextDigit;
      if NextDigit > 0 then
      begin
        Y.FBinExp := X.FBinExp;
        Y.FQWordDigits[Y.FQWordPrec-1].Hi := NextDigit shl (31-X.FBinExp);
        X.Diff(X,Y);
      end;
      X.Prod(X,numTen);
    end;
    NextDigit := DecimalDigit(X.FQWordDigits[X.FQWordPrec-1].Hi,X.FBinExp);
    if NextDigit >= 5 then
    begin
      Carry := IncDecimals(@Buffer,FDecPrec);
      if Carry then
      begin
        Buffer[0] := 1;
        Inc(TempDecExp);
      end;
    end;
    if TempDecExp > MaxDecExp then raise ENumOverflow.Create(SOverflow);
    if TempDecExp < -MaxDecExp
    then if (TempDecExp = -MaxDecExp-1) and (Buffer[0] >= 5)
         then begin
           Buffer[0] := 1;
           FillChar(Buffer[1],FDecPrec-1,0);
           Inc(TempDecExp);
         end
         else begin
           TempDecExp := 0;
           FillChar(Buffer,FDecPrec,0);
         end;
    Move(Buffer,FDecDigits^,FDecPrec);
    FDecExp := TempDecExp;
    FDecsValid := True;
  finally
    Y.Free;
    X.Free;
  end;
end;

function TNum.GetNonZeroDigits: Integer;
var
  i: Integer;
begin
  ValidateDecs;
  if FSign = Zero then
  begin
    Result := 0;
    Exit;
  end;
  Result := FDecPrec;
  for i := FDecPrec-1 downto 0 do
    if FDecDigits[i] = 0
    then Dec(Result)
    else Break;
end;

function DWordDigit(DecDigit: TDecDigit; var BinExp: Integer): DWord;
const
  DWordDigitsArray: array[TDecDigit] of DWord =
    (DWord($00000000),DWord($80000000),DWord($80000000),DWord($C0000000),
     DWord($80000000),DWord($A0000000),DWord($C0000000),DWord($E0000000),
     DWord($80000000),DWord($90000000));
  BinExpsArray: array[TDecDigit] of Integer = (-1,0,1,1,2,2,2,2,3,3);
begin
  BinExp := BinExpsArray[DecDigit];
  Result := DWordDigitsArray[DecDigit];
end;

procedure TNum.DoValidateQWords;
var
  X,Y: TNum;
  i,WorkPrec,DecDigitCount,DecExpInc: Integer;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  if FSign = Zero then
  begin
    ZeroQWords(FQWordDigits,FQWordPrec);
    FQWordsValid := True;
    Exit;
  end;
  X := nil;
  Y := nil;
  DecDigitCount := NonZeroDigits;
  { *10^(FDecExp - FDecPrec + 1) }
  DecExpInc := FDecExp - DecDigitCount + 1;
  WorkPrec := FDecPrec+DecLen(2*DecExpInc)+DecLen(2*DecDigitCount);
  try
    {$R-}
    X := TNum.Create(WorkPrec);
    Y := TNum.Create(MinDecPrec);
    {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
    Y.Copy(numOne);
    for i := 0 to DecDigitCount - 1 do
    begin
      if FDecDigits[i] <> 0 then
      begin
        Y.FQWordDigits[0].Hi := DWordDigit(FDecDigits[i],Y.FBinExp);
        X.Sum(X,Y);
      end;
      if i < DecDigitCount-1 then X.Prod(X,numTen);
    end;
    X.FSign := FSign;
    i := 0;
    if DecExpInc < 0
    then begin
      DecExpInc := -DecExpInc;
      repeat
        if Odd(DecExpInc) then X.Quot(X,Ten_P2_P[i]);
        Inc(i);
        DecExpInc := DecExpInc shr 1;
      until DecExpInc = 0;
    end
    else if DecExpInc > 0 then
         repeat
           if Odd(DecExpInc) then X.Prod(X,Ten_P2_P[i]);
           Inc(i);
           DecExpInc := DecExpInc shr 1;
         until DecExpInc = 0;
    Copy(X);
  finally
    Y.Free;
    X.Free;
  end;
  FQWordsValid := True;
end;

procedure TNum.ValidateDecs;
begin
  if not FDecsValid
  then if FQWordsValid
       then DoValidateDecs
       else raise ENum.Create(SInternal);
end;

procedure TNum.ValidateQWords;
begin
  if not FQWordsValid
  then if FDecsValid
       then DoValidateQWords
       else raise ENum.Create(SInternal);
end;

function TNum.GetAsString: string;
var
  i: Integer;
  ExpS: string;
begin
  SetLength(Result,FDecPrec+MaxDecExpLen+4);
  if Sign = Negative
  then Result[1] := '-'
  else Result[1] := ' ';
  Result[2] := DecChars[0];
  Result[3] := '.';
  for i := 1 to DecPrec-1 do
    Result[i+3] := DecChars[i];
  Result[DecPrec+3] := 'e';
  ExpS := Format('%*.*d',[MaxDecExpLen+1,MaxDecExpLen,DecExp]);
  if DecExp >= 0
  then ExpS[1] := '+';
  for i := 1 to MaxDecExpLen+1 do
    Result[DecPrec+3+i] := ExpS[i];
end;

function TNum.GetNonZeroBits: Integer;
var
  i,j,CurDWord: Integer;
begin
  ValidateQWords;
  if FSign = Zero then
  begin
    Result := 0;
    Exit;
  end;
  i := LeadingZeroCount(FQWordDigits,FQWordPrec);
  j := 0;
  CurDWord := FQWordDigits[i].Lo;
  if CurDWord = 0 then
  begin
    CurDWord := FQWordDigits[i].Hi;
    j := 32;
  end;
  while not Odd(CurDWord) do
  begin
    CurDWord := CurDWord shr 1;
    Inc(j);
  end;
  Result := (FQWordPrec-i)*64 - j;
end;

function TNum.GetAsInteger: Integer;
var
  Bits: Integer;
begin
  ValidateQWords;
  if Sign = Zero then
  begin
    Result := 0;
    Exit;
  end;
  Bits := NonZeroBits;
  if (BinExp >= Bits - 1) and (BinExp <= 30)
  then begin
    Result := FQWordDigits[FQWordPrec-1].Hi shr (31-BinExp);
    if Sign = Negative then Result := -Result;
  end
  else if (Sign = Negative) and (BinExp = 31) and (Bits = 1)
       then Result := MinDWord
       else raise ENumConvertError.CreateFmt(SNumConvertError,[AsFloat,'Integer']);
end;

function TNum.GetAsFloat: Extended;
var
  M: QWord;
  TempExp: Integer;
begin
  ValidateQWords;
  if Sign = Zero then
  begin
    Result := 0.0;
    Exit;
  end;
  M := FQWordDigits[FQWordPrec-1];
  TempExp := BinExp;
  if   (FQWordPrec > 1)
   and (FQWordDigits[FQWordPrec-2].Hi < 0)
   and (IncQWords(@M,1) <> 0) then
  begin
    M.Hi := MinDWord;
    Inc(TempExp);
  end;
  if TempExp > $3FFF
  then raise ENumConvertError.CreateFmt(SNumConvertError,[AsFloat,'Extended']);
  if TempExp < -$3FFF then
  begin
    Result := 0.0;
    Exit;
  end;
  with TExtendedRec(Result) do
  begin
    SignExp := TempExp + $3FFF;
    if Sign = Negative then SignExp := SignExp or -$8000;
    Mantissa := M;
  end;
end;

function TNum.GetAsInt64: Int64;
var
  Bits: Integer;
begin
  ValidateQWords;
  if Sign = Zero then
  begin
    Result := Int64(ZeroQWord);
    Exit;
  end;
  Bits := NonZeroBits;
  if (BinExp >= Bits - 1) and (BinExp <= 62)
  then begin
    {$IFDEF D4PLUS}
    Result := Int64(FQWordDigits[FQWordPrec-1]) shr (63-BinExp);
    if Sign = Negative then Result := -Result;
    {$ELSE}
    Result := FQWordDigits[FQWordPrec-1];
    NormalizeQWords(@Result,1);
    if Sign = Negative then
    begin
      {$R-}
      {$Q-}
      Result.Lo := not Result.Lo;
      Result.Hi := not Result.Hi;
      Inc(Result.Lo);
      if Result.Lo = 0 then Inc(Result.Hi);
      {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
      {$IFDEF ARKRNL_OVERFLOW} {$Q+} {$ENDIF}
    end;
    {$ENDIF}
  end
  else if (Sign = Negative) and (BinExp = 63) and (Bits = 1)
       then Result := MinInt64
       else raise ENumConvertError.CreateFmt(SNumConvertError,[AsFloat,'Int64']);
end;

function TNum.GetAsComp: Comp;
var
  Int: Int64;
begin
  Int := GetAsInt64;
  Move(Int,Result,SizeOf(Result));
end;

procedure TNum.Val_(var P: PChar; NumEnd0: Boolean);

  procedure SkipZeros;
  begin
    while P^ = '0' do Inc(P)
  end;

  function ReadExp: Integer;
  var
    StrExp: string;
    i: Integer;
    ExpSign: Boolean;
  begin
    StrExp := '';
    Inc(P);
    if P^ in ['+','-']
    then begin
      ExpSign := P^ = '-';
      Inc(P);
    end
    else ExpSign := False;
    i := 0;
    while (i < MaxDecExpLen) and (P^ in ['0'..'9']) do
    begin
      StrExp := StrExp + P^;
      Inc(P);
      Inc(i);
    end;
    if (i = 0) or (P^ in ['0'..'9']) then raise ENumValError.CreatePos(P);
    Result := StrToInt(StrExp);
    if ExpSign then Result := -Result;
    if (P^ <> #0) and NumEnd0 then raise ENumValError.CreatePos(P);
  end;

var
  NumTag, PointTag: Boolean;
  CurSym: PChar;
  TempSign: TSign;
  Counter,AExp,TempDecExp: Integer;
  Buffer: TDecDigits;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  ReallocMem(FDecDigits,FDecPrec);
  while (P^ <= ' ') and (P^ > #0) do Inc(P);
  Counter := 0;
  AExp := 0;
  PointTag := False;
  NumTag := False;
  TempDecExp := -1;
  if P^ = '+'
  then begin
    TempSign := Positive;
    Inc(P);
  end
  else if P^ = '-'
       then begin
         TempSign := Negative;
         Inc(P);
       end
       else if P^ in ['.','0'..'9']
            then TempSign := Positive
            else raise ENumValError.CreatePos(P);
  if P^ in ['0'..'9'] then
  begin
    NumTag := True;
    SkipZeros;
  end;
  if P^ = '.' then
  begin
    PointTag := True;
    Inc(P);
    if not (P^ in ['0'..'9'])
    then if NumTag
         then NumTag := False
         else raise ENumValError.CreatePos(P)
    else NumTag := True;
    CurSym := P;
    SkipZeros;
    Dec(TempDecExp,P-CurSym);
  end;
  if P^ in ['E','e'] then
    if NumTag
    then begin
      ReadExp;
      Sign := Zero;
      Exit;
    end
    else raise ENumValError.CreatePos(P);
  Dec(P);
  repeat
    Inc(P);
    case P^ of
      '0'..'9':
      begin
        NumTag := True;
        if not PointTag then Inc(TempDecExp);
        if Counter < FDecPrec then
        begin
          Buffer[Counter] := Ord(P^)-$30;
          if  (Counter = FDecPrec-1)
          and ((P+1)^ in ['0'..'9'])
          and ((P+1)^ >= '5')
          then if IncDecimals(@Buffer,Counter+1) then
               begin
                 Inc(TempDecExp);
                 Buffer[0] := 1;
               end;
          Inc(Counter);
        end;
      end;
      'E','e':
      begin
        if not NumTag then raise ENumValError.CreatePos(P);
        AExp := ReadExp;
        Break;
      end;
      '.':
      if PointTag
      then raise ENumValError.CreatePos(P)
      else PointTag := True;
      #0: Break;
      else if NumTag or PointTag
           then if NumEnd0
                then raise ENumValError.CreatePos(P)
                else Break
           else raise ENumValError.CreatePos(P);
    end;
  until False;
  if Buffer[0] = 0
  then TempSign := Zero
  else begin
    if FDecPrec > Counter
    then FillChar(Buffer[Counter],FDecPrec-Counter,0);
    Inc(TempDecExp,AExp);
    if TempDecExp < -MaxDecExp
    then if (Buffer[0] > 5) and (TempDecExp = -MaxDecExp)
         then begin
                Buffer[0] := 1;
                FillChar(Buffer[1],FDecPrec-1,0);
                Inc(TempDecExp);
              end
         else FillChar(Buffer,FDecPrec,0);
    if Buffer[0] = 0
    then TempSign := Zero;
  end;
  if TempSign = Zero then TempDecExp := 0;
  if TempDecExp > MaxDecExp then raise ENumOverflow.Create(SOverflow);
  FSign := TempSign;
  FDecExp := TempDecExp;
  Move(Buffer,FDecDigits^,FDecPrec);
  FDecsValid := True;
  FQWordsValid := False;
end;

procedure TNum.SetAsString(Value: string);
var
  P: PChar;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  Value := Trim(Value);
  P := PChar(Value);
  try
    Val_(P,True);
  except
    on E: ENumValError do
    begin
      E.FErrorPos := nil;
      E.Message := Format(SSetAsStringError,[Value]);
      raise;
    end;
  end;
end;

procedure TNum.SetAsInteger(Value: Integer);
var
  QWordVal: QWord;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  if Value = 0 then
  begin
    Sign := Zero;
    Exit;
  end;
  if Value < 0
  then begin
    Sign := Negative;
    if Value <> MinDWord then Value := -Value;
  end
  else Sign := Positive;
  ZeroQWords(FQWordDigits,FQWordPrec);
  QWordVal.Lo := 0;
  QWordVal.Hi := Value;
  FBinExp := 31 - NormalizeQWords(@QWordVal,1);
  FQWordDigits[FQWordPrec-1] := QWordVal;
  FQWordsValid := True;
  FDecsValid := False;
end;

procedure TNum.SetAsInt64(Value: Int64);
var
  QWordVal: QWord;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  if {$IFDEF D4PLUS} Value {$ELSE} (Value.Lo or Value.Hi) {$ENDIF} = 0 then
  begin
    Sign := Zero;
    Exit;
  end;
  if Value {$IFNDEF D4PLUS} .Hi {$ENDIF} < 0
  then begin
    Sign := Negative;
    {$IFDEF D4PLUS}
    if Value <> MinInt64 then Value := -Value;
    {$ELSE}
    if (Value.Lo <> 0) or (Value.Hi <> MinDWord) then
    begin
      {$R-}
      {$Q-}
      Value.Lo := not Value.Lo;
      Value.Hi := not Value.Hi;
      Inc(Value.Lo);
      if Value.Lo = 0 then Inc(Value.Hi);
      {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
      {$IFDEF ARKRNL_OVERFLOW} {$Q+} {$ENDIF}
    end;
    {$ENDIF}
  end
  else Sign := Positive;
  ZeroQWords(FQWordDigits,FQWordPrec);
  QWordVal := QWord(Value);
  FBinExp := 63 - NormalizeQWords(@QWordVal,1);
  FQWordDigits[FQWordPrec-1] := QWordVal;
  FQWordsValid := True;
  FDecsValid := False;
end;

procedure TNum.SetAsFloat(Value: Extended);
var
  InvalidStr: string;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  with TExtendedRec(Value) do
  begin
    if (SignExp and $7FFF) = $7FFF
    then begin
      if (Mantissa.Lo or Mantissa.Hi) = 0
      then if SignExp < 0
           then InvalidStr := '-INF'
           else InvalidStr := '+INF'
      else InvalidStr := 'NaN';
      raise ENumConvertError.CreateFmt(SFPUInvalidNumber,[InvalidStr]);
    end;
    if (Mantissa.Lo or Mantissa.Hi) = 0
    then Sign := Zero
    else begin
      if SignExp < 0
      then Sign := Negative
      else Sign := Positive;
      SignExp := SignExp and $7FFF;
      ZeroQWords(FQWordDigits,FQWordPrec-1);
      FQWordDigits[FQWordPrec-1] := Mantissa;
      FBinExp := SignExp - $3FFF - NormalizeQWords(@FQWordDigits[FQWordPrec-1],1);
      FQWordsValid := True;
      FDecsValid := False;
    end;
  end;
end;

procedure TNum.SetAsComp(Value: Comp);
var
  Int: Int64;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  Move(Value,Int,SizeOf(Int));
  SetAsInt64(Int);
end;

function TNum.GetIsInteger: Boolean;
var
  Bits: Integer;
begin
  Bits := NonZeroBits;
  Result := (Sign = Zero)
         or ((BinExp >= Bits - 1) and (BinExp <= 30))
         or ((Sign = Negative) and (BinExp = 31) and (Bits = 1));
end;

function TNum.GetIsInt64: Boolean;
var
  Bits: Integer;
begin
  Bits := NonZeroBits;
  Result := (Sign = Zero)
         or ((BinExp >= Bits - 1) and (BinExp <= 62))
         or ((Sign = Negative) and (BinExp = 63) and (Bits = 1));
end;

function TNum.GetIsReal: Boolean;
begin
  // not implemented yet
  Result := GetIsExtended;
end;

function TNum.GetIsSingle: Boolean;
begin
  // not implemented yet
  Result := GetIsExtended;
end;

function TNum.GetIsDouble: Boolean;
begin
  // not implemented yet
  Result := GetIsExtended;
end;

function TNum.GetIsExtended: Boolean;
var
  M: QWord; TempExp: Integer;
begin
  ValidateQWords;
  if Sign = Zero then
  begin
    Result := True;
    Exit;
  end;
  M := FQWordDigits[FQWordPrec-1];
  TempExp := BinExp;
  if   (FQWordPrec > 1)
   and (FQWordDigits[FQWordPrec-2].Hi < 0)
   and (IncQWords(@M,1) <> 0) then
  begin
    M.Hi := MinDWord;
    Inc(TempExp);
  end;
  Result := TempExp <= $3FFF;
end;

procedure RaiseListIndexError(Index: Integer);
begin
  {$IFDEF D3PLUS}
  raise EListError.CreateFmt(SListIndexError,[Index]);
  {$ELSE}
  raise EListError.CreateResFmt(SListIndexError,[Index]);
  {$ENDIF}
end;

function TNum.GetDecDigit(Index: Integer): TDecDigit;
begin
  ValidateDecs;
  if (Index >= 0) and (Index < FDecPrec)
  then Result := FDecDigits[Index]
  else begin
    RaiseListIndexError(Index);
    Result := 0; // compiler warning
  end;
end;

function TNum.GetDecChar(Index: Integer): TDecChar;
begin
  Result := Chr(GetDecDigit(Index)+$30);
end;

procedure TNum.SetDecDigit(Index: Integer; Digit: TDecDigit);
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  if Sign = Zero then Exit;
  if (Index >= 0) and (Index < FDecPrec)
  then begin
    ValidateDecs;
    if FDecDigits[Index] <> Digit then
    begin
      if (Index = 0) and (Digit = 0)
      then raise ENumInvalidArg.Create(SInvalidArg);
      FDecDigits[Index] := Digit;
      FQWordsValid := False;
    end;
  end
  else RaiseListIndexError(Index);
end;

procedure TNum.SetDecChar(Index: Integer; C: TDecChar);
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  SetDecDigit(Index,Ord(C)-$30);
end;

procedure TNum.SetDecPrec(ADecPrec: TDecPrec);
var
  NewQWordPrec: Integer;
  LastDigit: TDecDigit;
  BinCarry: Boolean;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  if FDecPrec <> ADecPrec then
  begin
    NewQWordPrec := DecsToQWords(ADecPrec);
    { Проверка на возможную ошибку (округление за пределы диапазона) }
    if FDecsValid and (ADecPrec < FDecPrec) then
    begin
      LastDigit := FDecDigits[ADecPrec];
      if   (LastDigit >= 5)
       and (FDecExp = MaxDecExp)
       and AllNines(FDecDigits,ADecPrec)
      then raise ENumOverflow.Create(SOverflow);
    end
    else LastDigit := 0;
    BinCarry := False;
    if not FDecsValid and (NewQWordPrec < FQWordPrec) then
    begin
      BinCarry := FQWordDigits[FQWordPrec-NewQWordPrec-1].Hi < 0;
      if   BinCarry
       and (FBinExp = MaxBinExp)
       and AllBitsSet(@FQWordDigits[FQWordPrec-NewQWordPrec],NewQWordPrec)
      then raise ENumOverflow.Create(SOverflow);
    end;
    { OK }
    if FDecsValid or ((ADecPrec < FDecPrec) and (FDecDigits <> nil))
    then ReallocMem(FDecDigits,ADecPrec);
    if FDecsValid and (ADecPrec > FDecPrec) then
    begin
      FillChar(FDecDigits[FDecPrec],ADecPrec-FDecPrec,0);
      FQWordsValid := False;
    end;
    if not FDecsValid and (NewQWordPrec > FQWordPrec) then
    begin
      ReallocMem(FQWordDigits,NewQWordPrec*8);
      CopyQWords(@FQWordDigits[NewQWordPrec-FQWordPrec],FQWordDigits,FQWordPrec);
      ZeroQWords(FQWordDigits,NewQWordPrec-FQWordPrec);
    end;
    if ADecPrec < FDecPrec then
    begin
      if   FDecsValid
       and (LastDigit >= 5)
       and IncDecimals(FDecDigits,ADecPrec) then
      begin
        FDecDigits[0] := 1;
        Inc(FDecExp);
      end;
      if   not FDecsValid
       and BinCarry
       and (IncQWords(@FQWordDigits[FQWordPrec-NewQWordPrec],NewQWordPrec) <> 0) then
      begin
        FQWordDigits[0].Hi := MinDWord;
        Inc(FBinExp);
      end;
      CopyQWords(FQWordDigits,@FQWordDigits[FQWordPrec-NewQWordPrec],NewQWordPrec);
      ReallocMem(FQWordDigits,NewQWordPrec*8);
    end;
    FQWordPrec := NewQWordPrec;
    FDecPrec := ADecPrec;
  end;
end;

function TNum.GetBinExp: Integer;
begin
  ValidateQWords;
  Result := FBinExp;
end;

procedure TNum.SetBinExp(ABinExp: TBinExp);
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  ValidateQWords;
  if ABinExp > MaxBinExp
  then raise ENumInvalidArg.Create(SInvalidArg);
  if FBinExp <> ABinExp
  then if ABinExp < -MaxBinExp
       then Sign := Zero
       else begin
         FBinExp := ABinExp;
         FDecsValid := False;
       end;
end;

function TNum.GetDecExp: TDecExp;
begin
  ValidateDecs;
  Result := FDecExp;
end;

procedure TNum.SetDecExp(ADecExp: TDecExp);
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  ValidateDecs;
  if ADecExp > MaxDecExp
  then raise ENumInvalidArg.Create(SInvalidArg);
  if FDecExp <> ADecExp
  then if ADecExp < -MaxDecExp
       then Sign := Zero
       else begin
         FDecExp := ADecExp;
         FQWordsValid := False;
       end;
end;

procedure TNum.SetSign(ASign: TSign);
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  if (FSign <> ASign) then
  begin
    FSign := ASign;
    if FSign = Zero then
    begin
      FBinExp := 0;
      ZeroQWords(FQWordDigits,FQWordPrec);
      FQWordsValid := True;
      if FDecsValid then
      begin
        FDecExp := 0;
        FillChar(FDecDigits^,FDecPrec,0);
      end;
    end;
  end;
end;

function TNum.GetQWordDigit(Index: Integer): QWord;
begin
  ValidateQWords;
  if (Index >= 0) and (Index < FQWordPrec)
  then Result := FQWordDigits[Index]
  else RaiseListIndexError(Index);
end;

procedure TNum.SetQWordDigit(Index: Integer; const Digit: QWord);
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  if FSign = Zero then Exit;
  if (Index >= 0) and (Index < FQWordPrec)
  then begin
    ValidateQWords;
    if  (FQWordDigits[Index].Lo <> Digit.Lo)
     or (FQWordDigits[Index].Hi <> Digit.Hi) then
    begin
      if (Index = FQWordPrec - 1) and (Digit.Hi >= 0)
      then raise ENumInvalidArg.Create(SInvalidArg);
      FQWordDigits[Index] := Digit;
      FDecsValid := False;
    end;
  end
  else RaiseListIndexError(Index);
end;

function DecLen(N: Integer): Integer;
var
  i: Integer;
begin
  if N < 10 then
  begin
    Result := 1;
    Exit;
  end;
  if N >= 1000000000 then
  begin
    Result := 10;
    Exit;
  end;
  if N >= 100000000 then
  begin
    Result := 9;
    Exit;
  end;
  i := 100;
  Result := 2;
  while N >= i do
  begin
    i := i*10;
    Inc(Result);
  end;
end;

procedure TNum.IntPower(X: TNum; Pow: Integer);
var
  m: Boolean;
  k,WorkPrec: Integer;
  Y: TNum;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  m := False;
  if Pow = 0 then
  begin
    if X.Sign = Zero then raise ENumInvalidArg.Create(SInvalidArg);
    Copy(numOne);
    Exit;
  end;
  if Pow < 0 then
  begin
    Pow := -Pow;
    m := True
  end;
  WorkPrec := DecPrec + DecLen(Pow) + 1;
  {$R-}
  Y := TNum.Create(WorkPrec);
  {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
  try
    Y.Copy(numOne);
    k := 1;
    if Pow >= $40000000 then k := $40000000
    else while k*2 <= Pow do k := k*2;
    try
      repeat
        Y.Prod(Y,Y);
        if Pow >= k then
        begin
          Dec(Pow,k);
          Y.Prod(Y,X);
        end;
        k := k shr 1
      until k = 0;
      if m
      then Quot(numOne,Y)
      else Copy(Y);
    except
      on ENumOverflow do
        if m then Sign := Zero
             else raise;
    end;
  finally
    Y.Free;
  end;
end;

procedure TNum.Power(X,Y: TNum);
var
  Z: TNum;
begin
  Z := nil;
  if IsConst then raise ENumConst.Create(SNumConst);
  if Y.IsInteger
  then IntPower(X,Y.AsInteger)
  else try
    Z := TNum.Create(DecPrec+1);
    Z.Ln_(X);
    Z.Prod(Z,Y);
    Exp_(Z);
  finally
    Z.Free;
  end;
end;

function TNum.Odd_: Boolean;
begin
  ValidateQWords;
  if (Sign = Zero) or (BinExp >= 64*FQWordPrec - 1)
  then Result := False
  else Result := BitSet(FQWordDigits,FQWordPrec,64*FQWordPrec-1-BinExp);
end;

procedure TNum.DivMod(X,Y,Remainder: TNum);
var
  WorkPrec: Integer;
  Z,U: TNum;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  if Y.Sign = Zero
  then if X.Sign = Zero
       then raise ENumInvalidArg.Create(SInvalidArg)
       else raise ENumDivByZero.Create(SDivByZero);
  WorkPrec := Trunc((X.BinExp-Y.BinExp+2)/Log_2_10) + 1;
  WorkPrec := IntMax(WorkPrec,MinDecPrec);
  if WorkPrec > MaxDecPrec + 6 { Потеря точности. Ничего не поделаешь :( }
  then WorkPrec := MaxDecPrec + 6;
  Z := nil;
  U := nil;
  try
    {$R-}
    Z := TNum.Create(WorkPrec);
    {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
    Z.Quot(X,Y);
    Z.Int_(Z);
    if (Remainder <> nil) and (Remainder <> Self) then
    begin
      WorkPrec := WorkPrec + Remainder.DecPrec + 1;
      if WorkPrec > MaxDecPrec + 6
      then WorkPrec := MaxDecPrec + 6;
      {$R-}
      U := TNum.Create(WorkPrec);
      {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
      U.Copy(Z);
      U.Prod(U,Y); { исключений не должно быть }
      Remainder.Diff(X,U);
    end;
    Copy(Z);
  finally
    U.Free;
    Z.Free;
  end;
end;

procedure TNum.Neg(X: TNum);
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  Copy(X);
  FSign := NegSign(FSign);
end;

procedure SinOrCos1(const Arg: TNum; Sinus: Boolean; var Result: TNum);
{ |Arg| <= Pi/4 ! }
var
  i,WorkPrec,CurPrec: Integer;
  A,B,D,E: TNum;
  Plus: Boolean;
begin
  WorkPrec := Result.DecPrec + DecLen(2*Arg.FBinExp) + 2;
  A := nil;
  B := nil;
  D := nil;
  E := nil;
  try
    {$R-}
    A := TNum.Create(WorkPrec);
    B := TNum.Create(WorkPrec);
    D := TNum.Create(WorkPrec);
    E := TNum.Create(8);
    {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
    Plus := False;
    A.Copy(numOne);
    B.Copy(numOne);
    D.Prod(Arg,Arg);
    { A - аккумулятор
      B = t^k/k!
      D = t^2
      E = k*(k-1) }
    i := 2;
    repeat
      if Odd(i) = Sinus then
      begin
        E.AsInteger := i*(i-1);
        B.Prod(B,D);
        B.Quot(B,E);
        if B.Sign = Zero then Break;
        CurPrec := WorkPrec + Trunc((B.BinExp+1)/Log_2_10);
        if CurPrec < -1 then Break;
        CurPrec := IntMax(CurPrec,MinDecPrec);
        {$R-}
        B.DecPrec := CurPrec;
        {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
        if Plus then A.Sum(A,B) else A.Diff(A,B);
        Plus := not Plus;
      end;
      Inc(i);
    until False;
    if Sinus then Result.Prod(A,Arg) else Result.Copy(A);
  finally
    E.Free;
    D.Free;
    B.Free;
    A.Free;
  end;
end;

procedure SinOrCos2(const Arg: TNum; Sinus: Boolean; var Result: TNum);
var
  YWorkPrec,ZWorkPrec: Integer;
  TempSign: TSign;
  Y,Z: TNum;
begin
  TempSign := Arg.Sign;
  if TempSign = Zero then
  begin
    if Sinus
    then Result.Sign := Zero
    else Result.Copy(numOne);
    Exit;
  end;
  ZWorkPrec := Trunc(Arg.BinExp/Log_2_10) + 2;
  ZWorkPrec := IntMax(MinDecPrec,ZWorkPrec);
  ZWorkPrec := IntMin(MaxDecPrec+6,ZWorkPrec);
  YWorkPrec := Result.DecPrec + 2;
  Y := nil;
  Z := nil;
  try
    {$R-}
    Y := TNum.Create(YWorkPrec);
    Z := TNum.Create(ZWorkPrec);
    {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
    Y.Abs_(Arg);
    Z.DivMod(Y,numPi,Y);
    if Z.Odd_ then TempSign := NegSign(TempSign); { (2k+1)*Pi + Y }
    if Y.Sign = Zero
    then if Sinus
         then Result.Sign := Zero
         else Result.Copy(numOne)
    else begin
      { 0 < Y < Pi }
      Inc(Y.FBinExp,2); { Мне можно так умножать на 4 :) }
      Z.DecPrec := MinDecPrec;
      Z.DivMod(Y,numPi,nil); { Z \in [0..3] }
      case Z.AsInteger of
        0: begin { 0 < Y < Pi/4 }
          Dec(Y.FBinExp,2);
          SinOrCos1(Y,Sinus,Result);
        end;
        1,2: begin { Pi/4 < Y < 3*Pi/4 }
          Dec(Y.FBinExp);
          Y.Diff(numPi,Y);
          if Y.Sign <> Zero then Dec(Y.FBinExp);
          SinOrCos1(Y,not Sinus,Result);
        end;
        3: begin { 3*Pi/4 < Y < Pi }
          Dec(Y.FBinExp,2);
          Y.Diff(Y,numPi);
          SinOrCos1(Y,Sinus,Result);
          Result.Neg(Result);
        end;
        else raise ENum.Create(SInternal);
      end;
    end;
    if Sinus
    then Result.Sign := MulSigns(Result.Sign,TempSign);
  finally
    Z.Free;
    Y.Free;
  end;
end;

procedure TNum.Sin_(X: TNum);
var
  CheckExp,CheckPrec: Integer;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  CheckExp := X.BinExp;
  CheckPrec := X.DecPrec;
  SinOrCos2(X,True,Self);
  CheckZero(CheckExp,CheckPrec);
end;

procedure TNum.Cos_(X: TNum);
var
  CheckExp,CheckPrec: Integer;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  CheckExp := X.BinExp;
  CheckPrec := X.DecPrec;
  SinOrCos2(X,False,Self);
  CheckZero(CheckExp,CheckPrec);
end;

procedure ArcTanConv(X,Result: TNum);
{ arctg x = 2 * arctg(result), result < x/2; x > 1/16 !!! }
var
  WorkPrec: Integer;
  Y: TNum;
begin
  WorkPrec := Result.DecPrec + 3;
  Y := nil;
  try
    {$R-}
    Y := TNum.Create(WorkPrec);
    {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
    Y.Prod(X,X);
    Y.Sum(Y,numOne);
    Y.Sqrt_(Y);
    Y.Diff(Y,numOne);
    Result.Quot(Y,X);
  finally
    Y.Free;
  end;
end;

procedure TNum.ArcTan_(X: TNum);
var
  i,j,WorkPrec,CurPrec: Integer;
  Y,Z,U,V,W: TNum;
  Plus,GTOne: Boolean;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  if X.Sign = Zero then
  begin
    Sign := Zero;
    Exit;
  end;
  if X.BinExp > Trunc(MaxDecPrec * Log_2_10) + 1 then
  begin
    Copy(numPi);
    Dec(FBinExp);
    Sign := X.Sign;
    Exit;
  end;
  WorkPrec := DecPrec + DecLen(16*QWordPrec + 16) + 1;
  Y := nil;
  Z := nil;
  U := nil;
  V := nil;
  W := nil;
  try
    {$R-}
    Y := TNum.Create(WorkPrec);
    Z := TNum.Create(WorkPrec);
    U := TNum.Create(WorkPrec);
    V := TNum.Create(WorkPrec);
    W := TNum.Create(WorkPrec);
    {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
    Plus := X.Sign = Positive;
    Y.Abs_(X);
    GTOne := Y.GT(numOne);
    if GTOne then Y.Quot(numOne,Y);
    j := 0;
    while Y.GTE(1/16) do
    begin
      ArcTanConv(Y,Y);
      Inc(j);
    end;
    Z.Prod(Y,Y);
    Z.Neg(Z);
    U.Copy(Y);
    W.Copy(Y);
    { Y - аргумент
      Z = -Y^2
      U = (-1)^i*Y^(2*i+1)
      V = U / (2*i+1)
      W - аккумулятор }
    i := 1;
    repeat
      U.Prod(U,Z);
      V.QuotE(U,2*i+1);
      if V.Sign = Zero then Break;
      CurPrec := WorkPrec + Trunc((V.BinExp+1)/Log_2_10);
      if CurPrec < -1 then Break;
      CurPrec := IntMax(CurPrec,MinDecPrec);
      {$R-}
      U.DecPrec := CurPrec;
      V.DecPrec := CurPrec;
      {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
      W.Sum(W,V);
      Inc(i);
    until False;
    Inc(W.FBinExp,j+1);
    if GTOne then W.Diff(numPi,W);
    Dec(W.FBinExp);
    if Plus then Copy(W) else Neg(W);
  finally
    W.Free;
    V.Free;
    U.Free;
    Z.Free;
    Y.Free;
  end;
end;

procedure TNum.Exp_(X: TNum);
var
  Y,Z,U: TNum;
  i,WorkPrec,CurPrec,SavedExp,SavedFrac: Integer;
  TempSign: TSign;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  TempSign := X.Sign;
  if TempSign = Zero then
  begin
    Copy(numOne);
    Exit;
  end;
  Y := nil;
  Z := nil;
  U := nil;
  try
    if X.GEE(LnMaxPlus) then raise ENumOverflow.Create(SOverflow);
    if X.LEE(LnMinPlus) then
    begin
      Sign := Zero;
      Exit;
    end;
    WorkPrec := DecPrec + DecLen(4*X.FBinExp) + 2;
    {$R-}
    Y := TNum.Create(WorkPrec);
    Z := TNum.Create(WorkPrec);
    U := TNum.Create(WorkPrec);
    {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
    { Y - аргумент
      Z = Y^i/i!
      U = аккумулятор }
    Y.Abs_(X);

    if Y.FBinExp < 0
    then SavedExp := -1
    else begin
      SavedExp := Y.FBinExp;
      Y.FBinExp := -1;
    end;
    Inc(Y.FBinExp,Exp_Count);
    SavedFrac := Y.IntTrunc;
    Y.Frac_(Y);

    U.Copy(numOne);
    if Y.Sign <> Zero then
    begin
      Dec(Y.FBinExp,Exp_Count);
      Z.Copy(Y);
      U.Sum(U,Z);
      i := 2;
      repeat
        Z.Prod(Z,Y);
        Z.QuotE(Z,i);
        if Z.Sign = Zero then Break;
        CurPrec := WorkPrec + Trunc((Z.BinExp+1)/Log_2_10);
        if CurPrec < -1 then Break;
        CurPrec := IntMax(CurPrec,MinDecPrec);
        {$R-}
        Z.DecPrec := CurPrec;
        {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
        U.Sum(U,Z);
        Inc(i);
      until False;
    end; { if Y.SignSign <> Zero ... }
    for i := -Exp_Count to -1 do
    begin
      if Odd(SavedFrac) then U.Prod(U,Exp2P[i]);
      SavedFrac := SavedFrac shr 1;
    end;
    IntPower(U,IntSign(TempSign)*(1 shl (SavedExp+1)));
  finally
    U.Free;
    Z.Free;
    Y.Free;
  end;
end;

procedure TNum.Ln_(X: TNum);
var
  i,Count,WorkPrec,CurPrec,SavedExp,SavedFracExp,CheckExp,CheckPrec: Integer;
  Y,Z,U,W: TNum;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  CheckExp := X.BinExp;
  CheckPrec := X.DecPrec;
  if X.Sign <> Positive
  then raise ENumInvalidArg.Create(SInvalidArg);
  Count := 16*QWordPrec + 16;
  WorkPrec := DecPrec + DecLen(2*Count) + 1;
  Y := nil;
  Z := nil;
  U := nil;
  W := nil;
  try
    {$R-}
    Y := TNum.Create(WorkPrec);
    Z := TNum.Create(WorkPrec);
    U := TNum.Create(WorkPrec);
    W := TNum.Create(WorkPrec);
    {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
    Y.Copy(X);
    SavedExp := X.BinExp;
    Y.FBinExp := 0;
    SavedFracExp := 0;
    for i := -1 downto -Two_Count do
    begin
      if Y.GE(Two_P2_P[i]) then
      begin
        Y.Quot(Y,Two_P2_P[i]);
        Inc(SavedFracExp);
      end;
      if i > -Two_Count then SavedFracExp := SavedFracExp shl 1;
    end;
    Y.Diff(Y,numOne);
    if Y.Sign <> Zero then
    begin
      { Y - аргумент
        Z = Y^(i-1)
        U = Z/i
        W = текущая сумма }
      Z.Copy(Y);
      U.Quot(Z,numTwo);
      W.Copy(numOne);
      W.Diff(W,U);
      for i := 3 to Count do
      begin
        Z.Prod(Z,Y);
        CurPrec := WorkPrec + Trunc((Z.BinExp+1)/Log_2_10);
        if CurPrec < -1 then Break;
        CurPrec := IntMax(CurPrec,MinDecPrec);
        {$R-}
        Z.DecPrec := CurPrec;
        U.DecPrec := CurPrec;
        {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
        U.QuotE(Z,i);
        if Odd(i) then W.Sum(W,U) else W.Diff(W,U);
      end;
      W.Prod(W,Y);
    end;
    Y.ProdE(Ln2,SavedExp + SavedFracExp / (1 shl Two_Count));
    W.Sum(W,Y);
    Copy(W);
    if (CheckExp < 1) and (CheckExp >= -1)
    then CheckZero(CheckExp,CheckPrec);
  finally
    W.Free;
    U.Free;
    Z.Free;
    Y.Free;
  end;
end;

procedure TNum.Abs_(X: TNum);
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  Copy(X);
  if Sign <> Zero then Sign := Positive;
end;

procedure TNum.Sqrt_(X: TNum);
var
  Y,Z: TNum;
  WorkPrec,CurPrec: Integer;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  if X.Sign = Negative
  then raise ENumInvalidArg.Create(SInvalidArg);
  if X.Sign = Zero then
  begin
    Sign := Zero;
    Exit;
  end;
  WorkPrec := DecPrec + 2;
  CurPrec := MinDecPrec;
  Y := nil;
  Z := nil;
  try
    {$R-}
    Y := TNum.Create(WorkPrec);
    Y.Copy(numOne);
    X.ValidateQWords;
    Y.FBinExp := X.FBinExp div 2;
    Z := TNum.Create(WorkPrec);
    repeat
      Y.DecPrec := CurPrec;
    {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
      Z.Copy(Y);
      Y.Quot(X,Y);
      Y.Sum(Z,Y);
      Y.Prod(Y,numHalf);
      Z.Diff(Y,Z);
      { Относительная точность < 2^(2*Z.FBinExp-X.FBinExp+1) }
      if CurPrec = WorkPrec then Break;
      if Z.Sign = Zero
      then CurPrec := IntMin(2*CurPrec,WorkPrec)
      else begin
        CurPrec := Trunc((2*X.BinExp-4*Z.BinExp-1)/Log_2_10)+5;
        CurPrec := IntMax(CurPrec,MinDecPrec);
        CurPrec := IntMin(CurPrec,WorkPrec);
      end;
    until False;
    Copy(Y);
  finally
    Z.Free;
    Y.Free;
  end;
end;

procedure TNum.Int_(X: TNum);
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  Copy(X);
  if (X.Sign = Zero) or (BinExp >= FQWordPrec*64-1) then Exit;
  if BinExp < 0
  then Sign := Zero
  else ResetBits(FQWordDigits,FQWordPrec,0,FQWordPrec*64-BinExp-2);
end;

procedure TNum.Frac_(X: TNum);
var
  NormCount: Integer;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  X.ValidateQWords;
  if X.BinExp >= X.FQWordPrec*64-1 then
  begin  { Заодно избегаем возможного переполнения при копировании }
    Sign := Zero;
    Exit;
  end;
  Copy(X);
  if Sign = Zero then Exit;
  if BinExp < 0 then Exit;
  ShiftUpQWords(FQWordDigits,FQWordPrec,1+FBinExp);
  FBinExp := -1;
  NormCount := NormalizeQWords(FQWordDigits,FQWordPrec);
  if NormCount < 0
  then Sign := Zero
  else Dec(FBinExp,NormCount);
end;

procedure TNum.Trunc_(X: TNum);
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  Int_(X);
end;

procedure TNum.Round_(X: TNum);
var
  Y: TNum;
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  Y := TNum.Create(X.DecPrec);
  try
    if X.Sign = Negative
    then Y.Diff(X,numHalf)
    else Y.Sum(X,numHalf);
    Trunc_(Y);
  finally
    Y.Free;
  end;
end;

procedure TNum.Ceil_(X: TNum);
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  Copy(X);
  if (X.Sign = Zero) or (BinExp >= FQWordPrec*64-1) then Exit;
  if BinExp < 0
  then Sign := Zero
  else begin
    //if IsAllBitsEqual(FQWordDigits,FQWordPrec,0,FQWordPrec*64-BinExp-2,False)
    //then Exit; { Уже целое }
    ResetBits(FQWordDigits,FQWordPrec,0,FQWordPrec*64-BinExp-2);
    if Sign = Positive
    then Sum(Self,numOne);
  end;
end;

procedure TNum.Floor_(X: TNum);
begin
  if IsConst then raise ENumConst.Create(SNumConst);
  Copy(X);
  if (X.Sign = Zero) or (BinExp >= FQWordPrec*64-1) then Exit;
  if BinExp < 0
  then Sign := Zero
  else begin
    //if IsAllBitsEqual(FQWordDigits,FQWordPrec,0,FQWordPrec*64-BinExp-2,False)
    //then Exit; { Уже целое }
    ResetBits(FQWordDigits,FQWordPrec,0,FQWordPrec*64-BinExp-2);
    if Sign = Negative
    then Diff(Self,numOne);
  end;
end;

function TNum.IntTrunc: Integer;
begin
  ValidateQWords;
  if Sign = Zero then
  begin
    Result := 0;
    Exit;
  end;
  if BinExp <= 30
  then begin
    if BinExp < 0
    then Result := 0
    else Result := FQWordDigits[FQWordPrec-1].Hi shr (31-BinExp);
    if Sign = Negative then Result := -Result;
  end
  else if   (Sign = Negative)
        and (BinExp = 31)
        and (FQWordDigits[FQWordPrec-1].Hi = MinDWord)
       then Result := MinDWord
       else raise ENumConvertError.CreateFmt(SNumConvertError,[AsFloat,'Integer']);
end;

function TNum.Int64Trunc: Int64;
begin
  ValidateQWords;
  if Sign = Zero then
  begin
    {$IFDEF D4PLUS}
    Result := 0;
    {$ELSE D4PLUS}
    Result.Lo := 0;
    Result.Hi := 0;
    {$ENDIF D4PLUS}
    Exit;
  end;
  if BinExp <= 62
  then begin
    if BinExp < 0
    then begin
      {$IFDEF D4PLUS}
      Result := 0;
      {$ELSE D4PLUS}
      Result.Lo := 0;
      Result.Hi := 0;
      {$ENDIF D4PLUS}
    end
    else begin
      {$IFDEF D4PLUS}
      Result := Int64(FQWordDigits[FQWordPrec-1]) shr (63-BinExp);
      {$ELSE D4PLUS}
      Result := FQWordDigits[FQWordPrec-1];
      ShiftDownQWords(@Result,1,63-BinExp);
      {$ENDIF D4PLUS}
    end;
    if Sign = Negative
    then begin
      {$IFDEF D4PLUS}
      Result := -Result;
      {$ELSE D4PLUS}
      Result.Lo := not Result.Lo;
      Result.Hi := not Result.Hi;
      IncQWords(@Result,1);
      {$ENDIF D4PLUS}
    end;
  end
  else if   (Sign = Negative)
        and (BinExp = 63)
       {$IFDEF D4PLUS}
        and (Int64(FQWordDigits[FQWordPrec-1]) = MinInt64)
       {$ELSE D4PLUS}
        and (FQWordDigits[FQWordPrec-1].Hi = MinDWord)
        and (FQWordDigits[FQWordPrec-1].Lo = 0)
       {$ENDIF D4PLUS}
       then Result := MinInt64
       else raise ENumConvertError.CreateFmt(SNumConvertError,[AsFloat,'Int64']);
end;

function TNum.IntRound: Integer;
var
  Y: TNum;
begin
  Y := TNum.Create(20);
  try
    if Sign = Negative
    then Y.Diff(Self,numHalf)
    else Y.Sum(Self,numHalf);
    Result := Y.IntTrunc;
  finally
    Y.Free;
  end;
end;

function TNum.IntCeil: Integer;
var
  Y: TNum;
begin
  Y := TNum.Create(20);
  try
    Y.Ceil_(Self);
    Result := Y.IntTrunc;
  finally
    Y.Free;
  end;
end;

function TNum.IntFloor: Integer;
var
  Y: TNum;
begin
  Y := TNum.Create(20);
  try
    Y.Floor_(Self);
    Result := Y.IntTrunc;
  finally
    Y.Free;
  end;
end;

function TNum.Int64Round: Int64;
var Y: TNum;
begin
  Y := TNum.Create(20);
  try
    if Sign = Negative
    then Y.Diff(Self,numHalf)
    else Y.Sum(Self,numHalf);
    Result := Y.Int64Trunc;
  finally
    Y.Free;
  end;
end;

function TNum.Int64Ceil: Int64;
var
  Y: TNum;
begin
  Y := TNum.Create(20);
  try
    Y.Ceil_(Self);
    Result := Y.Int64Trunc;
  finally
    Y.Free;
  end;
end;

function TNum.Int64Floor: Int64;
var
  Y: TNum;
begin
  Y := TNum.Create(20);
  try
    Y.Floor_(Self);
    Result := Y.Int64Trunc;
  finally
    Y.Free;
  end;
end;

procedure TNum.Str_(Size,APrec,Flags: Word; P: PChar);
begin
  // not implemented yet
end;

{ Функции для облегчения работы }

const
  MaxConsts = 10;
  FloatPrec = 20;

type
  TConstBuffer = array[0..1] of QWord;
  TConstBuffers = array[0..MaxConsts-1] of TConstBuffer;

var
  ConstBuffers: TConstBuffers;
  ConstBufferIndex: Integer;

function CreateE(Source: Extended): TNum;
begin
  Result := TNum.Create(FloatPrec);
  with Result do
  try
    AsFloat := Source;
    CopyQWords(@ConstBuffers[ConstBufferIndex],FQWordDigits,FQWordPrec);
    FIsConst := True;
    ReallocMem(FQWordDigits,0);
    FQWordDigits := @ConstBuffers[ConstBufferIndex];
    Inc(ConstBufferIndex);
    if ConstBufferIndex >= MaxConsts then
      raise ENum.Create(SInternalStackOverflow);
  except
    Free;
    raise;
  end;
end;

procedure DestroyE;
begin
  Dec(ConstBufferIndex);
  if ConstBufferIndex < 0 then
    raise ENum.Create(SInternalStackUnderflow);
end;

function CreateS(const Source: string; Prec: Integer): TNum;
begin
  {$R-}
  Result := TNum.Create(IntMin(MaxDecPrec+6,IntMax(Prec,Length(Source))));
  {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
  with Result do
  try
    AsString := Source;
  except
    Free;
    raise;
  end;
end;

function TNum.GTE(X: Extended): Boolean;
var
  Z: TNum;
begin
  Z := CreateE(X);
  try
    Result := GT(Z);
  finally
    DestroyE;
    Z.Free;
  end;
end;

function TNum.GTS(const X: string): Boolean;
var
  Z: TNum;
begin
  Z := CreateS(X,DecPrec);
  try
    Result := GT(Z);
  finally
    Z.Free;
  end;
end;

function TNum.LTE(X: Extended): Boolean;
var
  Z: TNum;
begin
  Z := CreateE(X);
  try
    Result := LT(Z);
  finally
    DestroyE;
    Z.Free;
  end;
end;

function TNum.LTS(const X: string): Boolean;
var
  Z: TNum;
begin
  Z := CreateS(X,DecPrec);
  try
    Result := LT(Z);
  finally
    Z.Free;
  end;
end;

function TNum.GEE(X: Extended): Boolean;
var
  Z: TNum;
begin
  Z := CreateE(X);
  try
    Result := GE(Z);
  finally
    DestroyE;
    Z.Free;
  end;
end;

function TNum.GES(const X: string): Boolean;
var
  Z: TNum;
begin
  Z := CreateS(X,DecPrec);
  try
    Result := GE(Z);
  finally
    Z.Free;
  end;
end;

function TNum.LEE(X: Extended): Boolean;
var
  Z: TNum;
begin
  Z := CreateE(X);
  try
    Result := LE(Z);
  finally
    DestroyE;
    Z.Free;
  end;
end;

function TNum.LES(const X: string): Boolean;
var
  Z: TNum;
begin
  Z := CreateS(X,DecPrec);
  try
    Result := LE(Z);
  finally
    Z.Free;
  end;
end;

function TNum.EqE(X: Extended): Boolean;
var
  Z: TNum;
begin
  Z := CreateE(X);
  try
    Result := Eq(Z);
  finally
    DestroyE;
    Z.Free;
  end;
end;

function TNum.EqS(const X: string): Boolean;
var
  Z: TNum;
begin
  Z := CreateS(X,DecPrec);
  try
    Result := Eq(Z);
  finally
    Z.Free;
  end;
end;

procedure TNum.SumE(X: TNum; Y: Extended);
var
  Z: TNum;
begin
  Z := CreateE(Y);
  try
    Sum(X,Z);
  finally
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.SumS(X: TNum; const Y: string);
var
  Z: TNum;
begin
  Z := CreateS(Y,X.DecPrec);
  try
    Sum(X,Z);
  finally
    Z.Free;
  end;
end;

procedure TNum.SumEE(X,Y: Extended);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateE(X);
    U := CreateE(Y);
    Sum(Z,U);
  finally
    DestroyE;
    U.Free;
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.SumES(X: Extended; const Y: string);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateE(X);
    U := CreateS(Y,FloatPrec);
    Sum(Z,U);
  finally
    U.Free;
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.SumSS(const X,Y: string);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateS(X,DecPrec);
    U := CreateS(Y,DecPrec);
    Sum(Z,U);
  finally
    U.Free;
    Z.Free;
  end;
end;

procedure TNum.ProdE(X: TNum; Y: Extended);
var
  Z: TNum;
begin
  Z := CreateE(Y);
  try
    Prod(X,Z);
  finally
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.ProdS(X: TNum; const Y: string);
var
  Z: TNum;
begin
  Z := CreateS(Y,X.DecPrec);
  try
    Prod(X,Z);
  finally
    Z.Free;
  end;
end;

procedure TNum.ProdEE(X,Y: Extended);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateE(X);
    U := CreateE(Y);
    Prod(Z,U);
  finally
    DestroyE;
    U.Free;
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.ProdES(X: Extended; const Y: string);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateE(X);
    U := CreateS(Y,FloatPrec);
    Prod(Z,U);
  finally
    U.Free;
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.ProdSS(const X,Y: string);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateS(X,DecPrec);
    U := CreateS(Y,DecPrec);
    Prod(Z,U);
  finally
    U.Free;
    Z.Free;
  end;
end;

procedure TNum.DiffE(X: TNum; Y: Extended);
var
  Z: TNum;
begin
  Z := CreateE(Y);
  try
    Diff(X,Z);
  finally
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.DiffS(X: TNum; const Y: string);
var
  Z: TNum;
begin
  Z := CreateS(Y,X.DecPrec);
  try
    Diff(X,Z);
  finally
    Z.Free;
  end;
end;

procedure TNum.DiffER(X: Extended; Y: TNum);
var
  Z: TNum;
begin
  Z := CreateE(X);
  try
    Diff(Z,Y);
  finally
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.DiffSR(const X: string; Y: TNum);
var
  Z: TNum;
begin
  Z := CreateS(X,Y.DecPrec);
  try
    Diff(Z,Y);
  finally
    Z.Free;
  end;
end;

procedure TNum.DiffEE(X,Y: Extended);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateE(X);
    U := CreateE(Y);
    Diff(Z,U);
  finally
    DestroyE;
    U.Free;
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.DiffES(X: Extended; const Y: string);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateE(X);
    U := CreateS(Y,FloatPrec);
    Diff(Z,U);
  finally
    U.Free;
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.DiffSE(const X: string; Y: Extended);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateS(X,FloatPrec);
    U := CreateE(Y);
    Diff(Z,U);
  finally
    DestroyE;
    U.Free;
    Z.Free;
  end;
end;

procedure TNum.DiffSS(const X,Y: string);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateS(X,DecPrec);
    U := CreateS(Y,DecPrec);
    Diff(Z,U);
  finally
    U.Free;
    Z.Free;
  end;
end;

procedure TNum.QuotE(X: TNum; Y: Extended);
var
  Z: TNum;
begin
  Z := CreateE(Y);
  try
    Quot(X,Z);
  finally
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.QuotS(X: TNum; const Y: string);
var
  Z: TNum;
begin
  Z := CreateS(Y,X.DecPrec);
  try
    Quot(X,Z);
  finally
    Z.Free;
  end;
end;

procedure TNum.QuotER(X: Extended; Y: TNum);
var
  Z: TNum;
begin
  Z := CreateE(X);
  try
    Quot(Z,Y);
  finally
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.QuotSR(const X: string; Y: TNum);
var
  Z: TNum;
begin
  Z := CreateS(X,Y.DecPrec);
  try
    Quot(Z,Y);
  finally
    Z.Free;
  end;
end;

procedure TNum.QuotEE(X,Y: Extended);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateE(X);
    U := CreateE(Y);
    Quot(Z,U);
  finally
    DestroyE;
    U.Free;
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.QuotES(X: Extended; const Y: string);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateE(X);
    U := CreateS(Y,FloatPrec);
    Quot(Z,U);
  finally
    U.Free;
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.QuotSE(const X: string; Y: Extended);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateS(X,FloatPrec);
    U := CreateE(Y);
    Quot(Z,U);
  finally
    DestroyE;
    U.Free;
    Z.Free;
  end;
end;

procedure TNum.QuotSS(const X,Y: string);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateS(X,DecPrec);
    U := CreateS(Y,DecPrec);
    Quot(Z,U);
  finally
    U.Free;
    Z.Free;
  end;
end;

procedure TNum.PowerE(X: TNum; Y: Extended);
var
  Z: TNum;
begin
  Z := CreateE(Y);
  try
    Power(X,Z);
  finally
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.PowerS(X: TNum; const Y: string);
var
  Z: TNum;
begin
  Z := CreateS(Y,X.DecPrec);
  try
    Power(X,Z);
  finally
    Z.Free;
  end;
end;

procedure TNum.PowerER(X: Extended; Y: TNum);
var
  Z: TNum;
begin
  Z := CreateE(X);
  try
    Power(Z,Y);
  finally
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.PowerSR(const X: string; Y: TNum);
var
  Z: TNum;
begin
  Z := CreateS(X,Y.DecPrec);
  try
    Power(Z,Y);
  finally
    Z.Free;
  end;
end;

procedure TNum.PowerEE(X,Y: Extended);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateE(X);
    U := CreateE(Y);
    Power(Z,U);
  finally
    DestroyE;
    U.Free;
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.PowerES(X: Extended; const Y: string);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateE(X);
    U := CreateS(Y,FloatPrec);
    Power(Z,U);
  finally
    U.Free;
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.PowerSE(const X: string; Y: Extended);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateS(X,FloatPrec);
    U := CreateE(Y);
    Power(Z,U);
  finally
    DestroyE;
    U.Free;
    Z.Free;
  end;
end;

procedure TNum.PowerSS(const X,Y: string);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateS(X,DecPrec);
    U := CreateS(Y,DecPrec);
    Power(Z,U);
  finally
    U.Free;
    Z.Free;
  end;
end;

procedure TNum.DivModE(X: TNum; Y: Extended; Remainder: TNum);
var
  Z: TNum;
begin
  Z := CreateE(Y);
  try
    DivMod(X,Z,Remainder);
  finally
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.DivModS(X: TNum; const Y: string; Remainder: TNum);
var
  Z: TNum;
begin
  Z := CreateS(Y,X.DecPrec);
  try
    DivMod(X,Z,Remainder);
  finally
    Z.Free;
  end;
end;

procedure TNum.DivModER(X: Extended; Y,Remainder: TNum);
var
  Z: TNum;
begin
  Z := CreateE(X);
  try
    DivMod(Z,Y,Remainder);
  finally
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.DivModSR(const X: string; Y,Remainder: TNum);
var
  Z: TNum;
begin
  Z := CreateS(X,Y.DecPrec);
  try
    DivMod(Z,Y,Remainder);
  finally
    Z.Free;
  end;
end;

procedure TNum.DivModEE(X,Y: Extended; Remainder: TNum);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateE(X);
    U := CreateE(Y);
    DivMod(Z,U,Remainder);
  finally
    DestroyE;
    U.Free;
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.DivModES(X: Extended; const Y: string; Remainder: TNum);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateE(X);
    U := CreateS(Y,FloatPrec);
    DivMod(Z,U,Remainder);
  finally
    U.Free;
    DestroyE;
    Z.Free;
  end;
end;

procedure TNum.DivModSE(const X: string; Y: Extended; Remainder: TNum);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateS(X,FloatPrec);
    U := CreateE(Y);
    DivMod(Z,U,Remainder);
  finally
    DestroyE;
    U.Free;
    Z.Free;
  end;
end;

procedure TNum.DivModSS(const X,Y: string; Remainder: TNum);
var
  Z,U: TNum;
begin
  Z := nil;
  U := nil;
  try
    Z := CreateS(X,DecPrec);
    U := CreateS(Y,DecPrec);
    DivMod(Z,U,Remainder);
  finally
    U.Free;
    Z.Free;
  end;
end;

procedure TNum.Sin_E(X: Extended);
var
  Y: TNum;
begin
  Y := CreateE(X);
  try
    Sin_(Y);
  finally
    DestroyE;
    Y.Free;
  end;
end;

procedure TNum.Sin_S(const X: string);
var
  Y: TNum;
begin
  Y := CreateS(X,DecPrec);
  try
    Sin_(Y);
  finally
    Y.Free;
  end;
end;

procedure TNum.Cos_E(X: Extended);
var
  Y: TNum;
begin
  Y := CreateE(X);
  try
    Cos_(Y);
  finally
    DestroyE;
    Y.Free;
  end;
end;

procedure TNum.Cos_S(const X: string);
var
  Y: TNum;
begin
  Y := CreateS(X,DecPrec);
  try
    Cos_(Y);
  finally
    Y.Free;
  end;
end;

procedure TNum.ArcTan_E(X: Extended);
var
  Y: TNum;
begin
  Y := CreateE(X);
  try
    ArcTan_(Y);
  finally
    DestroyE;
    Y.Free;
  end;
end;

procedure TNum.ArcTan_S(const X: string);
var
  Y: TNum;
begin
  Y := CreateS(X,DecPrec);
  try
    ArcTan_(Y);
  finally
    DestroyE;
    Y.Free;
  end;
end;

procedure TNum.Exp_E(X: Extended);
var
  Y: TNum;
begin
  Y := CreateE(X);
  try
    Exp_(Y);
  finally
    DestroyE;
    Y.Free;
  end;
end;

procedure TNum.Exp_S(const X: string);
var
  Y: TNum;
begin
  Y := CreateS(X,DecPrec);
  try
    Exp_(Y);
  finally
    Y.Free;
  end;
end;

procedure TNum.Ln_E(X: Extended);
var
  Y: TNum;
begin
  Y := CreateE(X);
  try
    Ln_(Y);
  finally
    DestroyE;
    Y.Free;
  end;
end;

procedure TNum.Ln_S(const X: string);
var
  Y: TNum;
begin
  Y := CreateS(X,DecPrec);
  try
    Ln_(Y);
  finally
    Y.Free;
  end;
end;

procedure TNum.Sqrt_E(X: Extended);
var
  Y: TNum;
begin
  Y := CreateE(X);
  try
    Sqrt_(Y);
  finally
    DestroyE;
    Y.Free;
  end;
end;

procedure TNum.Sqrt_S(const X: string);
var
  Y: TNum;
begin
  Y := CreateS(X,DecPrec);
  try
    Sqrt_(Y);
  finally
    Y.Free;
  end;
end;

procedure TNum.Int_E(X: Extended);
var
  Y: TNum;
begin
  Y := CreateE(X);
  try
    Int_(Y);
  finally
    DestroyE;
    Y.Free;
  end;
end;

procedure TNum.Trunc_E(X: Extended);
begin
  Int_E(X);
end;

procedure TNum.Round_E(X: Extended);
var
  Y: TNum;
begin
  Y := CreateE(X);
  try
    Round_(Y);
  finally
    DestroyE;
    Y.Free;
  end;
end;

procedure TNum.Ceil_E(X: Extended);
var
  Y: TNum;
begin
  Y := CreateE(X);
  try
    Ceil_(Y);
  finally
    DestroyE;
    Y.Free;
  end;
end;

procedure TNum.Floor_E(X: Extended);
var
  Y: TNum;
begin
  Y := CreateE(X);
  try
    Floor_(Y);
  finally
    DestroyE;
    Y.Free;
  end;
end;

{$IFDEF D4PLUS}

{ Функции для облегчения работы в Delphi 4+ }

function TNum.GT(X: Extended): Boolean;
begin
  Result := GTE(X);
end;

function TNum.GT(const X: string): Boolean;
begin
  Result := GTS(X);
end;

function TNum.LT(X: Extended): Boolean;
begin
  Result := LTE(X);
end;

function TNum.LT(const X: string): Boolean;
begin
  Result := LTS(X);
end;

function TNum.GE(X: Extended): Boolean;
begin
  Result := GEE(X);
end;

function TNum.GE(const X: string): Boolean;
begin
  Result := GES(X);
end;

function TNum.LE(X: Extended): Boolean;
begin
  Result := LEE(X);
end;

function TNum.LE(const X: string): Boolean;
begin
  Result := LES(X);
end;

function TNum.Eq(X: Extended): Boolean;
begin
  Result := EqE(X);
end;

function TNum.Eq(const X: string): Boolean;
begin
  Result := EqS(X);
end;

procedure TNum.Sum(X: TNum; Y: Extended);
begin
  SumE(X,Y);
end;

procedure TNum.Sum(X: TNum; const Y: string);
begin
  SumS(X,Y);
end;

procedure TNum.Sum(X,Y: Extended);
begin
  SumEE(X,Y);
end;

procedure TNum.Sum(X: Extended; const Y: string);
begin
  SumES(X,Y);
end;

procedure TNum.Sum(const X,Y: string);
begin
  SumSS(X,Y);
end;

procedure TNum.Prod(X: TNum; Y: Extended);
begin
  ProdE(X,Y);
end;

procedure TNum.Prod(X: TNum; const Y: string);
begin
  ProdS(X,Y);
end;

procedure TNum.Prod(X,Y: Extended);
begin
  ProdEE(X,Y);
end;

procedure TNum.Prod(X: Extended; const Y: string);
begin
  ProdES(X,Y);
end;

procedure TNum.Prod(const X,Y: string);
begin
  ProdSS(X,Y);
end;

procedure TNum.Diff(X: TNum; Y: Extended);
begin
  DiffE(X,Y);
end;

procedure TNum.Diff(X: TNum; const Y: string);
begin
  DiffS(X,Y);
end;

procedure TNum.Diff(X: Extended; Y: TNum);
begin
  DiffER(X,Y);
end;

procedure TNum.Diff(const X: string; Y: TNum);
begin
  DiffSR(X,Y);
end;

procedure TNum.Diff(X,Y: Extended);
begin
  DiffEE(X,Y);
end;

procedure TNum.Diff(X: Extended; const Y: string);
begin
  DiffES(X,Y);
end;

procedure TNum.Diff(const X: string; Y: Extended);
begin
  DiffSE(X,Y);
end;

procedure TNum.Diff(const X,Y: string);
begin
  DiffSS(X,Y);
end;

procedure TNum.Quot(X: TNum; Y: Extended);
begin
  QuotE(X,Y);
end;

procedure TNum.Quot(X: TNum; const Y: string);
begin
  QuotS(X,Y);
end;

procedure TNum.Quot(X: Extended; Y: TNum);
begin
  QuotER(X,Y);
end;

procedure TNum.Quot(const X: string; Y: TNum);
begin
  QuotSR(X,Y);
end;

procedure TNum.Quot(X,Y: Extended);
begin
  QuotEE(X,Y);
end;

procedure TNum.Quot(X: Extended; const Y: string);
begin
  QuotES(X,Y);
end;

procedure TNum.Quot(const X: string; Y: Extended);
begin
  QuotSE(X,Y);
end;

procedure TNum.Quot(const X,Y: string);
begin
  QuotSS(X,Y);
end;

procedure TNum.Power(X: TNum; Y: Extended);
begin
  PowerE(X,Y);
end;

procedure TNum.Power(X: TNum; const Y: string);
begin
  PowerS(X,Y);
end;

procedure TNum.Power(X: Extended; Y: TNum);
begin
  PowerER(X,Y);
end;

procedure TNum.Power(const X: string; Y: TNum);
begin
  PowerSR(X,Y);
end;

procedure TNum.Power(X,Y: Extended);
begin
  PowerEE(X,Y);
end;

procedure TNum.Power(X: Extended; const Y: string);
begin
  PowerES(X,Y);
end;

procedure TNum.Power(const X: string; Y: Extended);
begin
  PowerSE(X,Y);
end;

procedure TNum.Power(const X,Y: string);
begin
  PowerSS(X,Y);
end;

procedure TNum.DivMod(X: TNum; Y: Extended; Remainder: TNum);
begin
  DivModE(X,Y,Remainder);
end;

procedure TNum.DivMod(X: TNum; const Y: string; Remainder: TNum);
begin
  DivModS(X,Y,Remainder);
end;

procedure TNum.DivMod(X: Extended; Y,Remainder: TNum);
begin
  DivModER(X,Y,Remainder);
end;

procedure TNum.DivMod(const X: string; Y,Remainder: TNum);
begin
  DivModSR(X,Y,Remainder);
end;

procedure TNum.DivMod(X,Y: Extended; Remainder: TNum);
begin
  DivModEE(X,Y,Remainder);
end;

procedure TNum.DivMod(X: Extended; const Y: string; Remainder: TNum);
begin
  DivModES(X,Y,Remainder);
end;

procedure TNum.DivMod(const X: string; Y: Extended; Remainder: TNum);
begin
  DivModSE(X,Y,Remainder);
end;

procedure TNum.DivMod(const X,Y: string; Remainder: TNum);
begin
  DivModSS(X,Y,Remainder);
end;

procedure TNum.Sin_(X: Extended);
begin
  Sin_E(X);
end;

procedure TNum.Sin_(const X: string);
begin
  Sin_S(X);
end;

procedure TNum.Cos_(X: Extended);
begin
  Cos_E(X);
end;

procedure TNum.Cos_(const X: string);
begin
  Cos_S(X);
end;

procedure TNum.ArcTan_(X: Extended);
begin
  ArcTan_E(X);
end;

procedure TNum.ArcTan_(const X: string);
begin
  ArcTan_S(X);
end;

procedure TNum.Exp_(X: Extended);
begin
  Exp_E(X);
end;

procedure TNum.Exp_(const X: string);
begin
  Exp_S(X);
end;

procedure TNum.Ln_(X: Extended);
begin
  Ln_E(X);
end;

procedure TNum.Ln_(const X: string);
begin
  Ln_S(X);
end;

procedure TNum.Sqrt_(X: Extended);
begin
  Sqrt_E(X);
end;

procedure TNum.Sqrt_(const X: string);
begin
  Sqrt_S(X);
end;

procedure TNum.Int_(X: Extended);
begin
  Int_E(X);
end;

procedure TNum.Trunc_(X: Extended);
begin
  Int_E(X);
end;

procedure TNum.Round_(X: Extended);
begin
  Round_E(X);
end;

procedure TNum.Ceil_(X: Extended);
begin
  Ceil_E(X);
end;

procedure TNum.Floor_(X: Extended);
begin
  Floor_E(X);
end;

{$ENDIF}

{ Исключения }

function ENum.GetErrorCode: Integer;
begin
  // abstract
  Result := ENUM_UNKNOWN;
end;

function ENumConvertError.GetErrorCode: Integer;
begin
  Result := ENUM_CONVERT;
end;

function ENumDivByZero.GetErrorCode: Integer;
begin
  Result := ENUM_DIV_BY_ZERO;
end;

function ENumInvalidArg.GetErrorCode: Integer;
begin
  Result := ENUM_INVALID_ARG;
end;

function ENumOverflow.GetErrorCode: Integer;
begin
  Result := ENUM_OVERFLOW;
end;

constructor ENumValError.CreatePos(AnErrorPos: PChar);
begin
  Create(AnErrorPos+SNumValErrorPos);
  ErrorPos := AnErrorPos;
end;

constructor ENumValError.CreatePosMsg(AnErrorPos: PChar; const Msg: string);
begin
  Create(Msg);
  ErrorPos := AnErrorPos;
end;

function ENumValError.GetErrorCode: Integer;
begin
  Result := ENUM_VALUE;
end;

function ENumConst.GetErrorCode: Integer;
begin
  Result := ENUM_CONST;
end;

procedure CreateNumDefs(Dir,Name: string; Nums: TStrings);
const
  Comment = '// ';
var
  T: Text;
  i,j: Integer;
  Num: TNum;
  ListStr: string;
begin
  if not (Nums is TStrings) or (Nums.Count <= 0)
  then Exit;
  if Dir[Length(Dir)] = '\'
  then AssignFile(T,Dir+Name+'.pas')
  else AssignFile(T,Dir+'\'+Name+'.pas');
  Rewrite(T);
  try
    ListStr := Nums[0];
    for i := 1 to Nums.Count-1 do
      ListStr := ListStr + ',' + Nums[i];
    WriteLn(T,'unit ',Name,';');
    WriteLn(T,Comment,SNumDefs_FileCreated);
    WriteLn(T,Comment,SNumDefs_FileContains,ListStr);
    WriteLn(T,Comment,SNumDefs_UsageCommon1);
    WriteLn(T,Comment,SNumDefs_UsageCommon2);
    WriteLn(T,Comment,SNumDefs_Properties);
    WriteLn(T,Comment,SNumDefs_Then);
    WriteLn(T,Comment,SNumDefs_Param1);
    WriteLn(T,Comment,SNumDefs_Param2);
    WriteLn(T,Comment,SNumDefs_Param3);
    WriteLn(T,Comment,SNumDefs_Warning);
    WriteLn(T,Comment,SNumDefs_Example);
    WriteLn(T,Comment,'uses Classes, ArKrnl;');
    WriteLn(T,Comment);
    WriteLn(T,Comment,'var Num1,Num2: TNum; List: TStringList;');
    WriteLn(T,Comment);
    WriteLn(T,Comment,'begin');
    WriteLn(T,Comment,'  { ',SNumDefs_Create,'... }');
    WriteLn(T,Comment,'  List.AddObject(''Num1'',Num1);');
    WriteLn(T,Comment,'  List.AddObject(''SecondNum'',Num2);');
    WriteLn(T,Comment,'  CreateNumDefs(''C:\My Project\'',''Const_01'',List);');
    WriteLn(T,Comment,'  { ',SNumDefs_Destroy,'... }');
    WriteLn(T,Comment,'end.');
    WriteLn(T);
    WriteLn(T,'interface');
    WriteLn(T);
    WriteLn(T,'uses');
    WriteLn(T,'  ArKrnl;');
    WriteLn(T);
    WriteLn(T,'var');
    Write(T,'  ',ListStr);
    WriteLn(T,': TNum;');
    WriteLn(T);
    WriteLn(T,'implementation');
    WriteLn(T);
    WriteLn(T,'{$R-}'); { !!! }
    WriteLn(T);
    WriteLn(T,'type');
    WriteLn(T,'D = DWord;');
    WriteLn(T);
    WriteLn(T,'const');
    for i := 0 to Nums.Count-1 do
    begin
      Num := Nums.Objects[i] as TNum;
      WriteLn(T,
        Nums[i]+'_Digits: array[0..',
        IntToStr(Num.QWordPrec-1),
        '] of QWord =');
      Write(T,'  (');
      for j := 0 to Num.QWordPrec - 1 do
      begin
        Write(T,'(Lo: D($',IntToHex(Num.QWordDigits[j].Lo,8),');');
        Write(T,' Hi: D($',IntToHex(Num.QWordDigits[j].Hi,8),'))');
        if j < Num.QWordPrec - 1 then
        begin
          Write(T,',');
          if Odd(j)
          then Write(T,#$0D#$0A'   ');
        end;
      end;
      WriteLn(T,');');
      WriteLn(T);
    end;
    WriteLn(T,'CheatCompiler: Integer = MaxDecPrec;');
    WriteLn(T);
    WriteLn(T,'initialization');
    for i := 0 to Nums.Count-1 do
    begin
      Num := Nums.Objects[i] as TNum;
      Write(T,'  ',Nums[i],' := TNum.CreateConst(');
      if Num.DecPrec > MaxDecPrec
      then Write(T,'CheatCompiler+',
             IntToStr(Num.DecPrec-MaxDecPrec),',')
      else Write(T,IntToStr(Num.DecPrec),',');
      WriteLn(T,SignToStr(Num.Sign),',',
        IntToStr(Num.BinExp),',',
        '@'+Nums[i]+'_Digits);');
    end;
    WriteLn(T,'finalization');
    for i := Nums.Count-1 downto 0 do
      WriteLn(T,'  ',Nums[i],'.Destroy;');
    WriteLn(T,'end.');
  finally
    CloseFile(T);
  end;
end;

procedure InitializeArithKernel;
begin
  numZero := Const_00.numZero;
  numOne := Const_00.numOne;
  numMinusOne := Const_00.numMinusOne;
  numTwo := Const_00.numTwo;
  numHalf := Const_00.numHalf;
  numTen := Const_00.numTen;
  numPi := Const_00.numPi;
  ConstBufferIndex := 0;
end;

procedure FinalizeArithKernel;
begin
end;

initialization
  InitializeArithKernel;
finalization
  FinalizeArithKernel;
end.
