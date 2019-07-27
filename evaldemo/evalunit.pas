{
  arith - arbitrary precision floaing point library in Delphi.
  Demo calculator main unit and form
}
unit EvalUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ArKrnl, ArStacks, ExtCtrls;

type
  TTestForm = class(TForm)
    ExprLabel: TLabel;
    ExprMemo: TMemo;
    ResultMemo: TMemo;
    ResultLabel: TLabel;
    EvalButton: TButton;
    CloseButton: TButton;
    btnHistory: TButton;
    PrecLabel: TLabel;
    PrecEdit: TEdit;
    DecsLabel: TLabel;
    GradRadRG: TRadioGroup;
    FuncHelpLabel: TLabel;
    procedure btnHistoryClick(Sender: TObject);
    procedure EvalButtonClick(Sender: TObject);
    procedure PrecEditExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    CurPrec: TDecPrec;
  end;

var
  TestForm: TTestForm;

implementation

uses CalcHistory;

//uses ShellAPI;

{$R *.DFM}

procedure TTestForm.btnHistoryClick(Sender: TObject);
var
  s: string;
begin
  //ShellExecute(Handle,'open','arhelp.txt','','',SW_SHOWNORMAL);
  s := SelectExpression(ExprMemo.Text);
  if s <> ''
  then begin
    ExprMemo.Text := s;
    EvalButtonClick(EvalButton);
  end;
end;


procedure TTestForm.EvalButtonClick(Sender: TObject);
var
  S: string;
  N: TNum;
  ErrorPos: Word;
begin
  ActiveControl := ExprMemo;
  S := ExprMemo.Text;
  if S = '' then
  begin
    ResultMemo.Text := '';
    Exit;
  end;
  SaveExpression(S, True);
  N := nil;
  ExprMemo.SelStart := 0;
  ExprMemo.SelLength := Length(ExprMemo.Text);
  try
    try
      ResultMemo.Text := 'Ждите...';
      ExprMemo.Update;
      ResultMemo.Update;
      PrecEdit.Update;
      SetDeg(Boolean(GradRadRG.ItemIndex));
      N := ExprValue(PChar(S),CurPrec,ErrorPos);
      if ErrorPos <> Word(-1)
      then begin
        ExprMemo.SelStart := ErrorPos;
        ResultMemo.Text := 'Синтаксическая ошибка';
      end
      else begin
        S := N.AsString;
        if S[1] = ' ' then Delete(S,1,1);
        ResultMemo.Text := S;
      end;
    except
      on E: ENum do ResultMemo.Text := E.Message;
    end;
  finally
    N.Free;
  end;
end;

procedure TTestForm.PrecEditExit(Sender: TObject);
var
  NewPrec: Integer;
begin
  NewPrec := StrToIntDef(PrecEdit.Text,CurPrec);
  if (NewPrec >= Low(TDecPrec)) and (NewPrec <= High(TDecPrec))
  then CurPrec := NewPrec;
  PrecEdit.Text := IntToStr(CurPrec);
end;

procedure TTestForm.FormCreate(Sender: TObject);
begin
  CurPrec := 100;
end;

procedure TTestForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TTestForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveExpression(ExprMemo.Text, True);
end;

end.
