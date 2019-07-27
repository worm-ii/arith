{
  arith - arbitrary precision floaing point library in Delphi.
  Expression parsing stack (not a part of a library kernel)
}
unit ArStacks;

interface

uses ArKrnl;

type
  TAddNum = class(TNum)
    procedure Nop(A: TNum);
    procedure DgSin(A: TNum);
    procedure DgCos(A: TNum);
    procedure DgTan(A: TNum);
    procedure DgArcSin(A: TNum);
    procedure DgArcCos(A: TNum);
    procedure DgArcTan(A: TNum);
  end;

procedure SetDeg(Degrees: Boolean);
function ExprValue(P: PChar; Prec: Word; var ErrAddr: Word): TNum;

implementation

uses SysUtils, NumMsgs;

const
  DigitSet =  ['0'..'9'];
  SignSet = ['+','-'];
  ExpSet =  ['e','E'];
  LetterSet = ['A'..'Z','a'..'z'];
  Point = '.';
  AlphaSet = DigitSet + LetterSet;
  ValidSet = DigitSet + SignSet + ExpSet + [Point];
  EOS = #0;

  stOK         = 0;
  stOverflow   = 1;
  stUnderflow  = 2;

  MaxFnLen     = 6;

  StuffChars = [#1..' '];

  ENUM_PARSE = -101;

type

  UnaryFunc = procedure(A: TNum) of object;
  BinaryFunc = procedure(A,B: TNum) of object;

  TStack = class;
  PFnRec = ^TFnRec;

  TFnName = string[MaxFnLen];
  TFnRec = record
    Name: TFnName;
    Addr: Pointer;
    ArgC: Byte;
  end;

  TStcEl = class(TObject)
    Next: TStcEl;
    Owner: TStack;
    Data: Pointer;
    constructor Create;
    procedure   GetData(P: Pointer); virtual;
    procedure   SetData(P: Pointer); virtual;
    function    DataSize: Word; virtual;
  end;

  TStack = class(TObject)
    First: TStcEl;
    constructor Create(AFirst: TStcEl);
    constructor MakeEmpty;
    destructor  Destroy; override;
    procedure   Push(E: TStcEl);
    procedure   Drop;
    procedure   Pop(E: TStcEl);
    procedure   MoveTo(Dest: TStack);
    procedure   Clear;
    procedure   StackError(Code: Integer);
    function    Empty: Boolean;
    procedure   CheckEmpty;
  end;

  TFnEl = class(TStcEl)
    constructor Create(S: TFnName);
    function    DataSize: Word; override;
  end;

  TNumEl = class(TStcEl)
    constructor Create(P: TNum);
    destructor  Destroy; override;
    procedure   CopyTo(P: Pointer);
    procedure   CopyFrom(P: Pointer);
    procedure   GetData(P: Pointer); override;
    procedure   SetData(P: Pointer); override;
    function    DataSize: Word; override;
  end;

  EStack = class(Exception);

  ENumParseError = class(ENum)
  private
    FErrorPos: PChar;
  protected
    function GetErrorCode: Integer; override;
  public
    constructor CreatePos(AnErrorPos: PChar);
    constructor CreatePosMsg(AnErrorPos: PChar; const Msg: string);
    property ErrorPos: PChar read FErrorPos write FErrorPos;
  end;

const
  MaxSup = 18;
  afnrSupposed: array[1..MaxSup] of TFnRec =
  ((Name: '('     ; Addr: @TAddNum.Nop     ; ArgC: 0),
   (Name: '+'     ; Addr: @TNum.Sum        ; ArgC: 2),
   (Name: '-'     ; Addr: @TNum.Diff       ; ArgC: 2),
   (Name: '*'     ; Addr: @TNum.Prod       ; ArgC: 2),
   (Name: '/'     ; Addr: @TNum.Quot       ; ArgC: 2),
   (Name: '^'     ; Addr: @TNum.Power      ; ArgC: 2),
   (Name: 'PLUS'  ; Addr: @TAddNum.Nop     ; ArgC: 1),
   (Name: 'MINUS' ; Addr: @TNum.Neg        ; ArgC: 1),
   (Name: 'INT'   ; Addr: @TNum.Int_       ; ArgC: 1),
   (Name: 'SQRT'  ; Addr: @TNum.Sqrt_      ; ArgC: 1),
   (Name: 'EXP'   ; Addr: @TNum.Exp_       ; ArgC: 1),
   (Name: 'LN'    ; Addr: @TNum.Ln_        ; ArgC: 1),
   (Name: 'SIN'   ; Addr: @TAddNum.DgSin   ; ArgC: 1),
   (Name: 'COS'   ; Addr: @TAddNum.DgCos   ; ArgC: 1),
   (Name: 'TAN'   ; Addr: @TAddNum.DgTan   ; ArgC: 1),
   (Name: 'ARCSIN'; Addr: @TAddNum.DgArcSin; ArgC: 1),
   (Name: 'ARCCOS'; Addr: @TAddNum.DgArcCos; ArgC: 1),
   (Name: 'ARCTAN'; Addr: @TAddNum.DgArcTan; ArgC: 1));

var
  Degree: Boolean;

function StackErrorStr(Code: Word): string;
begin
  case Code of
    stOverflow: Result := SCalcStackOverflow;
    stUnderflow: Result := SCalcStackUnderflow;
  end;
end;

function GetIndex(S: TFnName): Integer;
var
  i: Integer;
begin
  for i := 1 to MaxSup do
    if afnrSupposed[i].Name = S then
    begin
      Result := i;
      Exit;
    end;
  Result := 0;
end;

procedure SetDeg;
begin
  Degree := Degrees;
end;

function GetDeg: Boolean;
begin
  Result := Degree;
end;

constructor TStack.Create;
begin
  inherited Create;
  First := AFirst;
  First.Owner := Self;
  First.Next := nil;
end;

constructor TStack.MakeEmpty;
begin
  inherited Create;
  First := nil;
end;

destructor TStack.Destroy;
begin
  inherited Destroy;
  Clear;
end;

procedure TStack.Clear;
begin
  while not Empty do Drop;
end;

function TStack.Empty;
begin
  Result := First = nil
end;

procedure TStack.Push;
var
  P: TStcEl;
begin
  P := First;
  First := E;
  First.Next := P;
  First.Owner := Self;
end;

procedure TStack.Pop;
begin
  CheckEmpty;
  First.SetData(E.Data);
  Drop;
end;

procedure TStack.Drop;
var
  P: TStcEl;
begin
  CheckEmpty;
  P := First.Next;
  First.Free;
  First := P;
end;

procedure TStack.MoveTo;
var
  P: TStcEl;
begin
  CheckEmpty;
  P:=First.Next;
  Dest.Push(First);
  First := P;
end;

procedure TStack.StackError;
begin
  raise EStack.Create(StackErrorStr(Code));
end;

procedure TStack.CheckEmpty;
begin
  if Empty then StackError(stUnderflow);
end;

constructor TStcEl.Create;
begin
  inherited Create;
  Data := nil;
  Owner := nil;
  Next := nil;
end;

function TStcEl.DataSize;
begin
  DataSize := SizeOf(Data);
end;

procedure TStcEl.GetData;
begin
  Move(P^,Data,DataSize);
end;

procedure TStcEl.SetData;
begin
  Move(Data,P^,DataSize);
end;


constructor TFnEl.Create;
var
  i: Integer;
begin
  inherited Create;
  i := GetIndex(S);
  if i = 0
  then raise ENumParseError.Create(SCalcSyntaxError);
  Data := Pointer(i);
end;

function TFnEl.DataSize;
begin
  DataSize := SizeOf(Integer);
end;


constructor TNumEl.Create;
begin
  inherited Create;
  Data := P;
end;

destructor TNumEl.Destroy;
begin
  TNum(Data).Free;
  inherited Destroy;
end;

function TNumEl.DataSize;
begin
  DataSize := SizeOf( (TNum(Data)) );
end;

procedure TNumEl.CopyTo;
begin
  TNum(P).Copy(TNum(Data));
end;

procedure TNumEl.CopyFrom;
begin
  TNum(Data).Copy(TNum(P));
end;

procedure TNumEl.GetData;
begin
  CopyFrom(Pointer(P^));
end;

procedure TNumEl.SetData;
begin
  CopyTo(Pointer(P^));
end;


procedure TAddNum.Nop;
begin
end;

procedure TAddNum.DgSin;
begin
  if not Degree then Sin_(A) else
  begin
    Prod(A,numPi);
    QuotE(Self,180);
    Sin_(Self);
  end;
end;


procedure TAddNum.DgCos;
begin
  if not Degree then Cos_(A) else
  begin
    Prod(A,numPi);
    QuotE(Self,180);
    Cos_(Self);
  end;
end;

procedure TAddNum.DgTan;
var
  S,C: TAddNum;
begin
  S := nil;
  C := nil;
  try
    {$R-}
    S := TAddNum.Create(DecPrec+1);
    C := TAddNum.Create(DecPrec+1);
    {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
    C.DgCos(A);
    if C.Sign = Zero then raise ENumInvalidArg.Create(SInvalidArg);
    S.DgSin(A);
    Quot(S,C);
  finally
    C.Free;
    S.Free;
  end;
end;

procedure TAddNum.DgArcTan;
begin
  if not Degree then ArcTan_(A) else
  begin
    ArcTan_(A);
    Quot(Self,numPi);
    ProdE(Self,180);
  end;
end;

procedure TAddNum.DgArcSin;
var
  Tmp: TAddNum;
begin
  Tmp := nil;
  try
    if A.GTE(1) or A.LTE(-1) then raise ENumInvalidArg.Create(SInvalidArg);
    if A.EqE(1) then
    begin
      if Degree then AsInteger := 90
                else QuotE(numPi,2);
      Exit;
    end;
    if A.EqE(-1) then
    begin
      if Degree then AsInteger := -90
                else QuotE(numPi,-2);
      Exit;
    end;
    {$R-}
    Tmp := TAddNum.Create(DecPrec+1);
    {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
    Tmp.Prod(A,A);
    Tmp.DiffER(1,Tmp);
    Tmp.Sqrt_(Tmp);
    Tmp.Quot(A,Tmp);
    DgArcTan(Tmp);
  finally
    Tmp.Free;
  end;
end;

procedure TAddNum.DgArcCos;
var
  Tmp: TAddNum;
begin
  Tmp := nil;
  try
    if A.GTE(1) or A.LTE(-1) then raise ENumInvalidArg.Create(SInvalidArg);
    if A.EqE(1) then
    begin
      Sign := Zero;
      Exit;
    end;
    if A.EqE(-1) then
    begin
      if Degree then AsInteger := 180
                else Copy(numPi);
      Exit;
    end;
    {$R-}
    Tmp := TAddNum.Create(DecPrec+1);
    {$IFDEF ARKRNL_RANGE} {$R+} {$ENDIF}
    Tmp.DgArcSin(A);
    if Degree
    then DiffER(90,Tmp)
    else
    begin
      Tmp.Sum(Tmp,Tmp);
      Tmp.Diff(numPi,Tmp);
      QuotE(Tmp,2);
    end;
  finally
    Tmp.Free;
  end;
end;

constructor ENumParseError.CreatePos(AnErrorPos: PChar);
begin
  Create(AnErrorPos+SNumValErrorPos);
  ErrorPos := AnErrorPos;
end;

constructor ENumParseError.CreatePosMsg(AnErrorPos: PChar; const Msg: string);
begin
  CreateFmt(Msg,[Pointer(AnErrorPos)]);
  ErrorPos := AnErrorPos;
end;

function ENumParseError.GetErrorCode: Integer;
begin
  Result := ENUM_PARSE;
end;

procedure SkipStuff(var PC: PChar);
begin
  while PC^ in StuffChars do Inc(PC);
end;

function FetchNum(var PC: PChar; var NI: TNum; Prec: Word): Boolean;
begin
  SkipStuff(PC);
  NI := TNum.Create(Prec);
  try
    NI.Val_(PC,False);
  except
    on E: ENumValError do
    begin
      PC := E.ErrorPos;
      NI.Free;
      NI := nil;
    end;
  end;
  Result := NI <> nil;
end;

function Prior(S: TFnName): Integer;
begin
  case S[1] of
    '+','-': Result := 1;
    '*','/': Result := 2;
    '^':     Result := 3;
    else Result := 0;
  end;
end;

procedure ReadSeq(var PC: PChar; var St: TStack; Prec: Word);
var
  NS: TStack;
  N: TNum;
  cCur: Char;
  sCur: TFnName;

  procedure NextAtom;
  begin
    Inc(PC);
    SkipStuff(PC);
  end;

  procedure Error;
  begin
    raise ENumParseError.CreatePosMsg(PC, SCalcSyntaxErrorAtAddr);
  end;

  function TopArgC: Byte;
  begin
    TopArgC := afnrSupposed[Integer(St.First.Data)].ArgC;
  end;

  function TopName: TFnName;
  begin
    TopName := afnrSupposed[Integer(St.First.Data)].Name;
  end;

  function PushFn(S: TFnName): Boolean;
  var
    P: TFnEl;
  begin
    try
      P := TFnEl.Create(S);
    except
      on ENumParseError do
        P := nil;
    end;
    Result := P <> nil;
    if Result then
    try
      St.Push(P);
    except
      P.Free;
      Result := False;
    end;
  end;

  function ReadOper: Boolean;
  var
    PC1: PChar;
  begin
    sCur := '';
    PC1 := PC;
    if PC1^ in LetterSet then
    begin
      sCur := UpCase(PC1^);
      Inc(PC1);
      while PC1^ in AlphaSet do
      begin
        if Length(sCur)<MaxFnLen
        then sCur := sCur+UpCase(PC1^)
        else Break;
        Inc(PC1);
      end;
      if (PC1^ in ValidSet) and not ((sCur='PI') or (PC1^ in ['+','-']))
      then begin sCur := ''; Result := False end
      else begin PC := PC1; Result := True end;
    end else Result := False;
  end;

begin
  NS := nil;
  try
    NS := TStack.MakeEmpty; St := TStack.MakeEmpty;
    with St do
    begin
      repeat
        SkipStuff(PC);
        while ReadOper or (PC^ in ['+','-','(']) do
        begin
          if sCur = '' then
          begin
            if (PC^ <> '(') and ((PC+1)^ in DigitSet+[Point]) then Break;
            case PC^ of
              '-': PushFn('MINUS');
              '(': PushFn('(');
            end;
            Inc(PC);
          end else
            if sCur = 'PI'
            then begin
              N := TNum.Create(Prec);
              N.Copy(numPi);
              Break;
            end
            else if not PushFn(sCur) then Error;
          SkipStuff(PC);
        end;
        if (sCur = 'PI') or FetchNum(PC,N,Prec) then
        begin
          NS.Push(TNumEl.Create(N));
          SkipStuff(PC); cCur := PC^;
          while cCur = ')' do
          begin
            while not Empty do
              if TopArgC = 0 then Break else MoveTo(NS);
            if Empty then Error;
            Drop;
            NextAtom;
            cCur := PC^;
          end;
          if cCur = EOS then Break;
          while not Empty and (TopArgC = 1) do MoveTo(NS);
          if cCur in ['+','-','*','/','^'] then
          begin
            NextAtom;
            while not Empty do
              if  (TopArgC = 1)
               or ((TopArgC = 2)
               and (Prior(TopName) >= Prior(cCur)))
              then MoveTo(NS) else Break;
            if not PushFn(cCur) then Error;
          end else Error;
        end
        else raise ENumParseError.CreatePos(PC);
      until False;
      while not Empty do
        if TopArgC <> 0 then MoveTo(NS) else Error;
    end;
    while not NS.Empty do NS.MoveTo(St);
  finally
    NS.Free;
  end;
end;

procedure Run(Elem: PFnRec; PS: TStack);
var
  MethodAddr: TMethod; { from SysUtils }
begin
  with Elem^, PS.First do
  begin
    MethodAddr.Code := Addr;
    if ArgC = 0 then Exit;
    if ArgC = 1
    then begin
      MethodAddr.Data := Data;
      UnaryFunc(MethodAddr)(Data)
    end
    else begin
      MethodAddr.Data := Next.Data;
      BinaryFunc(MethodAddr)(Next.Data,Data);
      PS.Drop;
    end;
  end;
end;

procedure CalcSeq(S: TStack; var N: TNum; Prec: Word);
var
  CalcS: TStack;
begin
  CalcS:=TStack.MakeEmpty;
  try
    if not S.Empty then
    repeat
      if S.First is TNumEl then S.MoveTo(CalcS) else
      begin
        if CalcS.Empty
        then raise ENumParseError.CreatePos(nil);
        Run(@afnrSupposed[Integer(S.First.Data)],CalcS);
        S.Drop;
      end;
    until S.Empty;
    N := TNum.Create(Prec);
    try
      N.Copy(TNum(CalcS.First.Data));
      CalcS.Drop;
    except
      N.Free;
      raise;
    end;
  finally
    CalcS.Free;
  end;
end;

function ExprValue(P: PChar; Prec: Word; var ErrAddr: Word): TNum;
var
  S: TStack;
  N: TNum;
  P1: PChar;
begin
  ErrAddr := Word(-1);
  S := nil;
  P1 := P;
  try
    try
      ReadSeq(P,S,Prec);
      CalcSeq(S,N,Prec);
    except
      on E: ENumParseError do
      begin
        if E.ErrorPos = nil
        then ErrAddr := P - P1
        else ErrAddr := E.ErrorPos - P1;
        Result := nil;
        Exit;
      end;
    end;
    Result := N;
  finally
    S.Free;
  end;
end;

initialization
  Degree := False;
end.
