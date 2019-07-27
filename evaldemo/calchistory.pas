{
  arith - arbitrary precision floaing point library in Delphi.
  Demo calculator history form
}
unit CalcHistory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmCalcList = class(TForm)
    lbExpressions: TListBox;
    lblExpressions: TLabel;
    memoSelExpr: TMemo;
    btnOK: TButton;
    btnClose: TButton;
    procedure lbExpressionsClick(Sender: TObject);
    procedure lbExpressionsDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  HistoryFName = 'EvalHist.dat';

var
  frmCalcList: TfrmCalcList;

  ExprHistory: TStringList = nil;

function SelectExpression(const CurExpression: string): string;

procedure SaveExpression(const CurExpression: string; SuppressErrors: Boolean);

implementation

{$R *.dfm}

{ TfrmCalcList }

procedure TfrmCalcList.lbExpressionsClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := lbExpressions.ItemIndex;
  if (idx >= 0) and (idx < lbExpressions.Items.Count)
  then memoSelExpr.Lines.Text := lbExpressions.Items[idx]
  else memoSelExpr.Lines.Text := '';
end;

procedure TfrmCalcList.lbExpressionsDblClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := lbExpressions.ItemIndex;
  if (idx >= 0) and (idx < lbExpressions.Items.Count)
  then begin
    ModalResult := mrOK
  end;
end;


const
  NeedSpaceChars = ['a'..'z','A'..'Z','0'..'9'];

function SpaceNeeded(const s: string; Index: Integer): Boolean;
begin
  if (Index <= 1) or (Index >= Length(S))
  then Result := False
  else Result := (S[Index-1] in NeedSpaceChars) and (S[Index+1] in NeedSpaceChars);
end;

function NormalizeExpression(const Expression: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Expression) do
  case Expression[i] of
    '0'..'9', '+', '-', '*', '/', '.', '(', ')', '^', 'a'..'z': Result := Result + Expression[i];
    #1..' ': if SpaceNeeded(Result + ' ' + Copy(Expression, i+1, 1), Length(Result) + 1)
             then Result := Result + ' ';
    'A'..'Z': Result := Result + Chr(Ord(Expression[i]) + (Ord('a')-Ord('A')));
  end;
  Result := Trim(Result);
end;


function HistoryFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + HistoryFName;
end;

function LoadExpressions(const FileName: string; sl: TStringList): Boolean;
var
  stream: TFileStream;
  s, s1: AnsiString;
  p, p1, p2: PAnsiChar;
begin
  Result := False;
  sl.Clear;
  if not FileExists(FileName)
  then Exit;
  stream := TFileStream.Create(FileName, fmOpenRead or fmShareExclusive);
  try
    SetLength(s, stream.Size+1);
    if Length(s) > 1
    then stream.Read(s[1],Length(s)-1);
    s[stream.Size+1] := #0;
  finally
    FreeAndNil(stream);
  end;
  if s = ''
  then Exit;
  p := PAnsiChar(s);
  p1 := p + Length(s);
  while p < p1 do
  begin
    p2 := StrEnd(p);
    if p^ <> #0
    then sl.Add(p);
    p := p2+1;
  end;
  Result := True;
end;

function SaveAllExpressions(const FileName: string; sl: TStringList): Boolean;
var
  stream: TFileStream;
  s: AnsiString;
  i: Integer;
begin
  Result := False;
  if sl.Count <= 0
  then s := ''
  else begin
    s := '';
    for i := 0 to sl.Count-1 do
    begin
      if sl[i] <> ''
      then begin
        if s <> ''
        then s := s + #0;
        s := s + sl[i];
      end;
    end;
  end;
  stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    if Length(s) > 0
    then stream.Write(s[1],Length(s));
  finally
    FreeAndNil(stream);
  end;
  Result := True;
end;

function SaveExpressions(const FileName: string; sl: TStringList): Boolean;
var
  sl1, sl2: TStringList;
  i, j: Integer;
  s: string;
begin
  if not FileExists(FileName)
  then begin
    Result := SaveAllExpressions(FileName, sl);
    Exit;
  end;
  sl1 := TStringList.Create;
  sl2 := TStringList.Create;
  try
    LoadExpressions(FileName, sl1);
    for i := 0 to sl1.Count-1 do
      sl1[i] := NormalizeExpression(sl1[i]);
    sl2.Sorted := True;
    sl2.Duplicates := dupIgnore;
    sl2.CaseSensitive := True;
    for i := 0 to sl1.Count-1 do
      sl2.Add(sl1[i]);
    for i := 0 to sl.Count-1 do
    begin
      s := NormalizeExpression(sl[i]);
      if not sl2.Find(s,j)
      then begin
        sl2.Add(s);
        sl1.Add(s);
      end;
    end;
    Result := SaveAllExpressions(FileName, sl1);
    sl.Clear;
    for i := 0 to sl1.Count-1 do
      sl.Add(sl1[i]);
  finally
    FreeAndNil(sl1);
    FreeAndNil(sl2);
  end;
end;

function SelectExpression(const CurExpression: string): string;
var
  norm: string;
  i, idx: Integer;
begin
  Result := '';
  norm := NormalizeExpression(CurExpression);
  if norm <> ''
  then ExprHistory.Add(norm);
  SaveExpressions(HistoryFileName, ExprHistory);
  if norm <> ''
  then begin
    i := ExprHistory.IndexOf(norm);
    if i <> ExprHistory.Count-1
    then begin
      if i < 0
      then ExprHistory.Add(norm)
      else ExprHistory.Exchange(i, ExprHistory.Count-1);
    end;
  end;
  frmCalcList := TfrmCalcList.Create(nil);
  try
    for i := ExprHistory.Count-1 downto 0 do
      if ExprHistory[i] = ''
      then ExprHistory.Delete(i);
    frmCalcList.lbExpressions.Items.Assign(ExprHistory);
    idx := ExprHistory.Count-1;
    if norm <> ''
    then begin
      for i := ExprHistory.Count-1 downto 0 do
      begin
        if ExprHistory[i] = norm
        then begin
          idx := i;
          Break;
        end;
      end;
    end;
    frmCalcList.lbExpressions.ItemIndex := idx;
    frmCalcList.lbExpressionsClick(frmCalcList.lbExpressions);
    frmCalcList.ActiveControl := frmCalcList.lbExpressions;
    if frmCalcList.ShowModal = mrOK
    then begin
      if frmCalcList.lbExpressions.ItemIndex >= 0
      then Result := frmCalcList.lbExpressions.Items[frmCalcList.lbExpressions.ItemIndex]
      else Result := '';
    end;
  finally
    FreeAndNil(frmCalcList);
  end;
end;

procedure SaveExpression(const CurExpression: string; SuppressErrors: Boolean);
var
  norm: string;
  i, idx: Integer;
begin
  try
    norm := NormalizeExpression(CurExpression);
    if norm = ''
    then Exit;
    ExprHistory.Add(norm);
    SaveExpressions(HistoryFileName, ExprHistory);
    i := ExprHistory.IndexOf(norm);
    if i <> ExprHistory.Count-1
    then begin
      if i < 0
      then ExprHistory.Add(norm)
      else ExprHistory.Exchange(i, ExprHistory.Count-1);
      SaveExpressions(HistoryFileName, ExprHistory);
    end;
  except
    if not SuppressErrors
    then raise;
  end;
end;

initialization
  ExprHistory := TStringList.Create;
finalization
  FreeAndNil(ExprHistory);
end.
