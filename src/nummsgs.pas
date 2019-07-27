{ 
  arith - arbitrary precision floaing point library in Delphi.
  Messages for internationalization
}
unit NumMsgs;

interface

{$IFDEF D3PLUS}
resourcestring
{$ELSE}
const
{$ENDIF}

  SInternal = '���������� ������';
  SInternalStackOverflow = '������������ ����������� �����';
  SInternalStackUnderflow = '������ ������� ����������� �����';
  SOverflow = '������������';
  SInvalidArg = '�������� ��������';
  SDivByZero = '������� �� 0';
  SNumValErrorPos = ' - ������ ��� ������ ������� ������� ���� ������';
  SSetAsStringError = '%s - �������� ��������� ������������� �����';
  SNumConvertError = '%e �� ������ � �������� ���� %s';
  SFPUInvalidNumber = '�� ���� ��������� �������������� %s';
  SNumConst = '������� ��������� �������� ���������� ��������';

  SNumDefs_FileCreated =
  '���� ���� ��� ������������� ������ ���������� CreateNumDefs.';
  SNumDefs_FileContains =
  '�������� �������� ��������: ';
  SNumDefs_UsageCommon1 =
  '����� ������� ������ � ��������� �������������� ����������� ��������,';
  SNumDefs_UsageCommon2 =
  '�������������� �������� TStringList (������ Classes).';
  SNumDefs_Properties =
  '�������� Objects ������ ��������� �����, � Strings - �� ��������������.';
  SNumDefs_Then =
  '����� �������� ��������� CreateNumDefs � �����������: ';
  SNumDefs_Param1 =
  '1-� �������� - ��� �����, � ������� �������� ������';
  SNumDefs_Param2 =
  '2-� �������� - ��� ������';
  SNumDefs_Param3 =
  '3-� �������� - ������ �����, �������������� �� ������������� ��������.';
  SNumDefs_Warning =
  '��������! ��������� �� ��������� ������������ ������������ ����������.';
  SNumDefs_Example =
  '������:';
  SNumDefs_Create =
  '������������� ����������';
  SNumDefs_Destroy =
  '����������� ����������';

  SCalcStackOverflow = '������������ �����';
  SCalcStackUnderflow = '������ ����';
  SCalcSyntaxError = '�������������� ������';
  SCalcSyntaxErrorAtAddr = '������ � ���������� �� ������ %p';

implementation

end.
