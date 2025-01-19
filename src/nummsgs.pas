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
  SInternal = 'Внутренняя ошибка';
  SInternalStackOverflow = 'Переполнение внутреннего стека';
  SInternalStackUnderflow = 'Чтение пустого внутреннего стека';
  SOverflow = 'Переполнение';
  SInvalidArg = 'Неверный аргумент';
  SDivByZero = 'Деление на 0';
  SNumValErrorPos = ' - ошибка при чтении первого символа этой строки';
  SSetAsStringError = '%s - неверное строковое представление числа';
  SNumConvertError = '%e не входит в диапазон типа %s';
  SFPUInvalidNumber = 'Не могу присвоить сопроцессорное %s';
  SNumConst = 'Попытка присвоить значение постоянной величине';

  SNumDefs_FileCreated =
  'Этот файл был автоматически создан процедурой CreateNumDefs.';
  SNumDefs_FileContains =
  'Содержит описание констант: ';
  SNumDefs_UsageCommon1 =
  'Чтобы создать модуль с описанием предварительно вычисленных констант,';
  SNumDefs_UsageCommon2 =
  'воспользуйтесь объектом TStringList (модуль Classes).';
  SNumDefs_Properties =
  'Свойство Objects должно содержать числа, а Strings - их идентификаторы.';
  SNumDefs_Then =
  'Затем вызовите процедуру CreateNumDefs с параметрами: ';
  SNumDefs_Param1 =
  '1-й параметр - имя папки, в которой создаётся модуль';
  SNumDefs_Param2 =
  '2-й параметр - имя модуля';
  SNumDefs_Param3 =
  '3-й параметр - список чисел, подготовленный по вышеописанным правилам.';
  SNumDefs_Warning =
  'Внимание! Процедура не проверяет корректность передаваемых параметров.';
  SNumDefs_Example =
  'Пример:';
  SNumDefs_Create =
  'инициализация переменных';
  SNumDefs_Destroy =
  'уничтожение переменных';

  SCalcStackOverflow = 'Переполнение стека';
  SCalcStackUnderflow = 'Пустой стек';
  SCalcSyntaxError = 'Синтаксическая ошибка';
  SCalcSyntaxErrorAtAddr = 'Ошибка в синтаксисе по адресу %p';

implementation

end.
