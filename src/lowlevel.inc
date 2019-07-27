{ 
  arith - arbitrary precision floaing point library in Delphi.
  32-bit x86 assembly and other low-level code.
}
const
  ZeroQWord: QWord = (Lo: 0; Hi: 0);
  MinusOneQWord: QWord = (Lo: -1; Hi: -1);

{$R-}
{$Q-}

{
procedure LongMul(Multiplicand,Multiplier: DWord; Product: PQWord); register;
// Product := (беззнаковое 64-битное целое) (Multiplicand*Multiplier)
asm
        mul     edx
        mov     [ecx],eax
        mov     [ecx+4],edx
end;
}

function AddQWord(Dest, Src: PQWordDigits): Integer; register;
{ Dest[0] += Src[0] (64-битное сложение);
  Result := [Был перенос из старшего бита результата: Да = -1; Нет: 0] }
asm
        push    esi
        push    edi
        mov     edi,[eax]
        mov     esi,[eax+4]
        add     edi,[edx]
        adc     esi,[edx+4]
        mov     [eax],edi
        mov     [eax+4],esi
        sbb     eax,eax
        pop     edi
        pop     esi
end;

function AddQWords(Dest, Src: PQWordDigits; Count: Cardinal): Integer; register;
{ Dest[0..Count-1] += Src[0..Count-1] ( (Count*64)-битное сложение );
  младшие байты в начале, старшие - в конце.
  Result := [Был перенос из старшего бита результата: Да = -1; Нет: 0] }
asm
        push    esi
        push    edi
        lea     eax,[eax+8*ecx]
        lea     edx,[edx+8*ecx]
        neg     ecx
        sub     esi,esi
@@Loop:
        mov     edi,[eax+8*ecx]
        mov     esi,[eax+8*ecx+4]
        adc     edi,[edx+8*ecx]
        adc     esi,[edx+8*ecx+4]
        mov     [eax+8*ecx],edi
        mov     [eax+8*ecx+4],esi
        inc     ecx
        jnz     @@Loop
        sbb     eax,eax
        pop     edi
        pop     esi
end;

function SubQWord(Dest, Src: PQWordDigits): Integer; register;
{ Dest[0] += Src[0] (64-битное вычитание);
  Result := [Был заём из старшего бита результата: Да = -1; Нет: 0] }
asm
        push    esi
        push    edi
        mov     edi,[eax]
        mov     esi,[eax+4]
        sub     edi,[edx]
        sbb     esi,[edx+4]
        mov     [eax],edi
        mov     [eax+4],esi
        sbb     eax,eax
        pop     edi
        pop     esi
end;

function SubQWords(Dest, Src: PQWordDigits; Count: Cardinal): Integer; register;
{ Dest[0..Count-1] += Src[0..Count-1] ( (Count*64)-битное вычитание);
  младшие байты в начале, старшие - в конце.
  Result := [Был заём из старшего бита результата: Да = -1; Нет: 0] }
asm
        push    esi
        push    edi
        lea     eax,[eax+8*ecx]
        lea     edx,[edx+8*ecx]
        neg     ecx
        sub     esi,esi
@@Loop:
        mov     edi,[eax+8*ecx]
        mov     esi,[eax+8*ecx+4]
        sbb     edi,[edx+8*ecx]
        sbb     esi,[edx+8*ecx+4]
        mov     [eax+8*ecx],edi
        mov     [eax+8*ecx+4],esi
        inc     ecx
        jnz     @@Loop
        sbb     eax,eax
        pop     edi
        pop     esi
end;

function IncQWords(Dest: PQWordDigits; Count: Cardinal): Integer; register;
{ Dest[0..Count-1] ++;
  Result := [Был перенос из старшего бита результата: Да = -1; Нет: 0] }
asm
        shl     edx,1
        jz      @@Exit
        lea     eax,[eax+4*edx]
        neg     edx
@@Loop:
        add     dword ptr [eax+4*edx],1
        jnc     @@Exit
        inc     edx
        jnz     @@Loop
@@Exit:
        sbb     eax,eax
end;

function DecQWords(Dest: PQWordDigits; Count: Cardinal): Integer; register;
{ Dest[0..Count-1] --;
  Result := [Был заём из старшего бита результата: Да = -1; Нет: 0] }
asm
        shl     edx,1
        jz      @@Exit
        lea     eax,[eax+4*edx]
        neg     edx
@@Loop:
        sub     dword ptr [eax+4*edx],1
        jnc     @@Exit
        inc     edx
        jnz     @@Loop
@@Exit:
        sbb     eax,eax
end;

procedure CopyQWords(Dest,Src: PQWordDigits; Count: Cardinal);
{ Dest[0..Count-1] := Src[0..Count-1]; }
begin
  Move(Src^,Dest^,Count*8);
end;

function MulDAddQWords(Mult: DWord; Dest,Src: PQWordDigits; Count: Integer): Integer; pascal;
{ Dest[0..Count-1/2] (участвуют Count четверных слов и 1 двойное слово)
    += Mult*Src[0..Count-1];
  Result := [Был перенос из старшего бита результата: Да = -1; Нет: 0] }
asm
        mov     eax,Count
        test    eax,eax
        jle     @@Exit
        push    esi
        push    edi
        push    ebx
        mov     edi,Dest
        mov     esi,Src
        mov     ecx,Count
        lea     esi,[esi+8*ecx]
        lea     edi,[edi+8*ecx]
        neg     ecx
        sub     eax,eax

@@EvenLoop:
        rcr     ebx,1
        mov     eax,Mult
        mul     dword ptr [esi+8*ecx]
        rcl     ebx,1
        adc     [edi+8*ecx],eax
        adc     [edi+8*ecx+4],edx
        inc     ecx
        jnz     @@EvenLoop

        pushf

        mov     ecx,Count
        neg     ecx
        sub     eax,eax

@@OddLoop:
        rcr     ebx,1
        mov     eax,Mult
        mul     dword ptr [esi+8*ecx+4]
        rcl     ebx,1
        adc     [edi+8*ecx+4],eax
        adc     [edi+8*ecx+8],edx
        inc     ecx
        jnz     @@OddLoop

        jnc     @@NoCarry
        popf
        adc     dword ptr [edi],0
        stc
        jmp     @@OK
@@NoCarry:
        popf
        adc     dword ptr [edi],0
@@OK:
        pop     ebx
        pop     edi
        pop     esi
@@Exit:
        sbb     eax,eax
end;

function MulDSubQWords(Quot: DWord; Dest,Src: PQWordDigits; Count: Cardinal): Integer; pascal;
{ Dest[0..Count-1/2] (участвуют Count четверных слов и 1 двойное слово)
    -= Quot*Src[0..Count-1];
  Result := [Был заём из старшего бита результата: Да = -1; Нет: 0] }
asm
        mov     eax,Count
        test    eax,eax
        jle     @@Exit
        push    esi
        push    edi
        push    ebx
        mov     edi,Dest
        mov     esi,Src
        mov     ecx,Count
        lea     esi,[esi+8*ecx]
        lea     edi,[edi+8*ecx]
        neg     ecx
        sub     eax,eax

@@EvenLoop:
        rcr     ebx,1
        mov     eax,Quot
        mul     dword ptr [esi+8*ecx]
        rcl     ebx,1
        sbb     [edi+8*ecx],eax
        sbb     [edi+8*ecx+4],edx
        inc     ecx
        jnz     @@EvenLoop

        pushf

        mov     ecx,Count
        neg     ecx
        sub     eax,eax

@@OddLoop:
        rcr     ebx,1
        mov     eax,Quot
        mul     dword ptr [esi+8*ecx+4]
        rcl     ebx,1
        sbb     [edi+8*ecx+4],eax
        sbb     [edi+8*ecx+8],edx
        inc     ecx
        jnz     @@OddLoop

        jnc     @@NoCarry
        popf
        sbb     dword ptr [edi],0
        stc
        jmp     @@OK
@@NoCarry:
        popf
        sbb     dword ptr [edi],0
@@OK:
        pop     ebx
        pop     edi
        pop     esi
@@Exit:
        sbb     eax,eax
end;

function LeadingZeroCount(P: PQWordDigits; Count: Integer): Integer;
{ Выдаёт количество нулевых четверных слов, идущих в начале массива P
  длиной Count элементов }
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if (P[i].Lo or P[i].Hi) = 0
    then Inc(Result)
    else Break;
end;

function TrailingZeroCount(P: PQWordDigits; Count: Integer): Integer;
{ Выдаёт количество нулевых четверных слов, идущих в конце массива P
  длиной Count элементов }
var
  i: Integer;
begin
  Result := 0;
  for i := Count-1 downto 0 do
    if (P[i].Lo or P[i].Hi) = 0
    then Inc(Result)
    else Break;
end;

procedure FillQWords(P: PQWordDigits; Count: Integer; Data: QWord);
{ Заполняет массив P длиной Count элементов элементом Data }
var
  i: Integer;
begin
  for i := 0 to Count-1 do P[i] := Data;
end;

procedure ZeroQWords(P: PQWordDigits; Count: Integer);
{ Обнуляет массив P длиной Count элементов }
begin
  FillChar(P^,Count*8,0);
end;

function AreQWordsZero(P: PQWordDigits; Count: Integer): Boolean;
{ Проверяет на 0 массив P длиной Count элементов }
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count-1 do
    if Int64(P[i]) <> 0
    then Exit;
  Result := True;
end;

procedure SwapInt(var I1,I2: Integer);
begin
  I1 := I1 xor I2;
  I2 := I2 xor I1;
  I1 := I1 xor I2;
end;

procedure SwapQWordPtrs(var P1,P2: PQWordDigits);
begin
  P1 := Pointer(Integer(P1) xor Integer(P2));
  P2 := Pointer(Integer(P2) xor Integer(P1));
  P1 := Pointer(Integer(P1) xor Integer(P2));
end;

function CmpQWords(A1,A2: PQWordDigits; Len1,Len2: Integer): Integer;
{ Сравнивает 2 массива длинами Len1 и Len2 64-битных беззнаковых
  целых чисел в каждом.
  Результат: 0 - массивы совпадают;
            -1 - A1 < A2
             1 - A1 > A2
  (младшие байты идут в начале, старшие - в конце) }
var
  i,j: Integer;
  SwapData: Boolean;
begin
  Result := 0;
  SwapData := Len1 < Len2;
  if SwapData then
  begin
    SwapQWordPtrs(A1,A2);
    SwapInt(Len1,Len2);
  end;
  for i := Len1 - 1 downto Len1 - Len2 do
  begin
    j := i - Len1 + Len2;
    if A1[i].Hi = A2[j].Hi
    then if A1[i].Lo = A2[j].Lo
         then Continue
         else begin
           if Cardinal(A1[i].Lo) > Cardinal(A2[j].Lo)
           then Result := 1
           else Result := -1;
           Break;
         end
    else begin
      if Cardinal(A1[i].Hi) > Cardinal(A2[j].Hi)
      then Result := 1
      else Result := -1;
      Break;
    end;
  end;
  if Result = 0
  then Result := Integer(TrailingZeroCount(A1,Len1-Len2) < Len1-Len2);
  if SwapData then Result := - Result;
end;

procedure MulQWords(Md,Mr,Prod: PQWordDigits; MdP,MrP,PP: Integer);
{
  Эта процедура осуществляет низкоуровневое умножение 2-х чисел, невзирая
    на позицию плавающей точки.
  На входе: Md,Mr,Prod указывают на три массива, представляющих
    соответственно множимое, множитель и произведение длиной
    соответственно MdP, MrP и PP четверных слов.
    Эти массивы рассматриваются как мантиссы чисел.
    Внимание! Должны выполняться неравенства:
      2^31 <= Md[MdP-1].Hi, Mr[MrP-1],Hi <= 2^32 (нормализация мантисс).
      Это гарантирует нам то, что мы вычислим не меньше бит результата,
      чем хотели бы.
  На выходе: Приближённое произведение множимого и множителя помещается
    по адресу PP. Его старший бит может быть равен нулю, но в любом случае
    старшая "цифра" (как беззнаковое 64-битное целое) будет >= 2^62.
    Младшая "цифра" Prod[0] в общем случае неверна. У предпоследней по
    значимости "цифры" Prod[1] могут быть неверны младшие Log2(PP)+2 бит,
    т.е. реальная точность результата - 64*PP-Log2(PP)-66 бит.
}
var
  i,DP: Integer;
begin
  ZeroQWords(Prod,PP);
  i := LeadingZeroCount(Md,MdP);
  Md := @Md[i];
  MdP := MdP-i;
  i := LeadingZeroCount(Mr,MrP);
  Mr := @Mr[i];
  MrP := MrP-i;
  DP  := MdP+MrP-PP;
  for i := 0 to MrP-1 do
  begin
    if i < DP then
    begin
      MulDAddQWords(Mr[i].Lo,@Prod[0].Lo,@Md[DP-i].Lo,MdP+i-DP);
      MulDAddQWords(Mr[i].Hi,@Prod[0].Hi,@Md[DP-i].Lo,MdP+i-DP);
    end
    else begin
      MulDAddQWords(Mr[i].Lo,@Prod[i-DP].Lo,@Md[0].Lo,MdP);
      MulDAddQWords(Mr[i].Hi,@Prod[i-DP].Hi,@Md[0].Lo,MdP);
    end;
  end;
end;

const
  cf2P31 = 2.0*(1 shl 30); { 2^31 }
  cf2P32 = 2.0*cf2P31; { 2^32 }
  cf2P64 = cf2P32*cf2P32;  { 2^64 }

function FloatToDWord(Value: Extended): DWord;
{ Результат - беззнаковое значение Value, округлённое вниз
  Подразумевается, что 0 <= Value <= 2^32. Проверок не производится ! }
begin
  if Value = cf2P32
  then Result := -1
  else if Value < 1
       then Result := 0
       else with TExtendedRec(Value) do
              Result := Mantissa.Hi shr ($3FFF + $1F - SignExp);
end;

procedure DivQWords(Dvd,Dvs,Q: PQWordDigits; QP: Integer);
{
  Эта процедура осуществляет низкоуровневое деление 2-х чисел, невзирая на
    позицию плавающей точки.
  Число представляется в виде массива из, скажем, Count "цифр".
    "Цифрой" мы здесь называем 64-битное беззнаковое целое, располагающееся
    в 8-ми последовательных байтах памяти, причём младшие байты идут
    в начале (имеют меньший адрес), а старшие - в конце. Аналогично, сами
    "цифры" идут в том же порядке: младшие в начале, старшие - в конце.
    Значение такого числа X можно определить как
      ( X[0]+2^64*X[1]+2^128*X[2]+...+(2^(64*(Count-1)))*X[Count-1] ) * 2^K,
    где масштабный множитель 2^K определяет положение плавающей точки внутри
    мантиссы и нас в контексте данной процедуры не интересует.
  На входе: Dvd, Dvs - указатели на делимое и делитель соответственно,
    каждое число длины QP, младшие "цифры" - вначале, старшие - в конце
    (т.е. Dvd[QP-1],Dvs[QP-1] - наиболее значащие "цифры").
    Внимание! Должны выполняться неравенства:
      2^31 <= Dvs[QP-1].Hi, Dvd[QP-1].Hi < 2^32 (нормализация мантисс).
      Это гарантирует нам то, что частное будет находиться в диапазоне
      1/2 < Q < 2 и мы вычислим не меньше бит мантиссы, чем хотели бы.
      Для выполнения этого условия достаточно домножить делимое и делитель
      на степень двойки (нули данной процедурой не допускаются).
    Внимание! Перед и после делимого и делителя должны стоять
      нулевые двойные слова!
  На выходе: QP ~= Dvd/Dvs, старшая "цифра" Q[QP-1] = 0 или 1;
    у младшей "цифры" Q[0] могут быть неверны младшие Log2(QP)+1 бит,
    т.е. реальная точность результата - 64*QP-Log2(QP)-65 бит.
}
var
  QD: DWord;
  DvdQ: QWord;
  DvdE,DvsE: Extended;
  QDbl: Double;
  DvsP,i: Integer;
begin
  Q[QP-1].Hi := 0;
  if CmpQWords(Dvd,Dvs,QP,QP) >= 0
  then begin
    SubQWords(Dvd,Dvs,QP);
    Q[QP-1].Lo := 1;
  end
  else Q[QP-1].Lo := 0;
  DvsP := QP - LeadingZeroCount(Dvs,QP);
  Dvs := @Dvs[QP-DvsP];
  DvsE := Comp(Dvs[DvsP-1])+cf2P64;
  for i := QP-2 downto 0 do
  begin
    DvdQ := Dvd[i+1];
    if DvdQ.Hi < 0
    then DvdE := Comp(DvdQ)+cf2P64
    else DvdE := Comp(DvdQ);
    QDbl := DvdE*cf2P32/DvsE;
    QD := FloatToDWord(QDbl);
    if i+2 > DvsP
    then if MulDSubQWords(QD,@Dvd[i+1-DvsP].Hi,@Dvs[0],DvsP) <> 0
         then begin
           AddQWords(@Dvd[i+1-DvsP].Hi,@Dvs[0],DvsP);
           Dec(QD);
         end
         else
    else if MulDSubQWords(QD,@Dvd[0],@Dvs[DvsP-i-2].Hi,i+2) <> 0
         then begin
           AddQWords(@Dvd[0],@Dvs[DvsP-i-2].Hi,i+2);
           Dec(QD);
         end;
    Q[i].Hi := QD;
    DvdQ.Hi := Dvd^[i+1].Lo;
    DvdQ.Lo := Dvd^[i].Hi;
    if DvdQ.Hi < 0
    then DvdE := Comp(DvdQ)+cf2P64
    else DvdE := Comp(DvdQ);
    QDbl := DvdE*cf2P32/DvsE;
    QD := FloatToDWord(QDbl);
    if i+2 > DvsP
    then if MulDSubQWords(QD,@Dvd[i+1-DvsP],@Dvs[0],DvsP) <> 0
         then begin
           AddQWords(@Dvd[i+1-DvsP],@Dvs[0],DvsP);
           Dec(QD);
         end
         else
    else if MulDSubQWords(QD,@Dvd[0],@Dvs[DvsP-i-1],i+1) <> 0
         then begin
           AddQWords(@Dvd[0],@Dvs[DvsP-i-1],i+1);
           Dec(QD);
         end;
    Q[i].Lo := QD;
  end;
end;


function _ShiftUpQWords(P: PQWordDigits; Count, ShiftBitsCount: Integer): Integer;
{ Сдвигает "агрегат" из Count четверных слов, адресуемый указателем P, вверх
 на (ShiftBitsCount mod 32) бит. Возвращает старшее двойное слово результата }
asm
        dec     edx
        js      @@Exit
        push    edx
        test    ecx,$1F
        jz      @@Return
        push    esi
        push    edi
        push    ebx
        test    edx,edx
        jz      @@EndOfLoop

@@Loop:
        mov     ebx,[eax+8*edx-4]
        mov     edi,[eax+8*edx]
        mov     esi,[eax+8*edx+4]
        shld    esi,edi,cl
        shld    edi,ebx,cl
        mov     [eax+8*edx],edi
        mov     [eax+8*edx+4],esi
        dec     edx
        jnz     @@Loop

@@EndOfLoop:
        sub     ebx,ebx
        mov     edi,[eax]
        mov     esi,[eax+4]
        shld    esi,edi,cl
        shld    edi,ebx,cl
        mov     [eax],edi
        mov     [eax+4],esi

        pop     ebx
        pop     edi
        pop     esi
@@Return:
        pop     edx
        mov     eax,[eax+8*edx+4]
@@Exit:
end;

function ShiftUpQWords(P: PQWordDigits; Count, ShiftBitsCount: Integer): Integer;
{ Сдвигает "агрегат" из Count четверных слов, адресуемый указателем P, вверх
 на ShiftBitsCount бит. Возвращает старшее двойное слово результата }
var
  DWordCount: Integer;
begin
  Count := Count shl 1;
  DWordCount := ShiftBitsCount shr 5;
  if DWordCount > Count then DWordCount := Count;
  if DWordCount > 0 then
  begin
    Move(P^,PDWordDigits(P)[DWordCount],4*(Count-DWordCount));
    FillChar(P^,DWordCount*4,0);
  end;
  if Count > DWordCount
  then Result := _ShiftUpQWords(@PDWordDigits(P)[DWordCount and $FFFFFFFE],
                                (Count-DWordCount+1) div 2,ShiftBitsCount)
  else Result := 0;
end;

function _NormShiftCount(const Q: QWord): Integer;
{ Возвращает количество битовых сдвигов вверх, которые необходимо произвести
 над Q, чтобы установился старший бит, или -1, если Q = 0 }
var
  QHi, QLo: DWord;
begin
  QHi := Q.Hi;
  QLo := Q.Lo;
  if (QHi or QLo) = 0
  then Result := -1
  else if QHi = 0
       then begin
         Result := 32;
         while QLo > 0 do begin
           QLo := QLo shl 1;
           Inc(Result);
         end;
       end
       else begin
         Result := 0;
         while QHi > 0 do
         begin
           QHi := QHi shl 1;
           Inc(Result);
         end;
       end;
end;

function NormShiftCount(P: PQWordDigits; Count: Integer): Integer;
{ Возвращает количество битовых сдвигов вверх, которые необходимо произвести
 над "агрегатом" P из Count четверных слов, чтобы установился старший бит,
 или -1, если P = 0 }
var
  Zeros: Integer;
begin
  Zeros := TrailingZeroCount(P,Count);
  if Zeros = Count
  then Result := -1
  else Result := _NormShiftCount(P[Count - Zeros - 1]) + Zeros shl 6;
end;

function NormalizeQWords(P: PQWordDigits; Count: Integer): Integer;
{ Сдвигает "агрегат" из Count четверных слов, адресуемый указателем Data, вверх
 до установления старшего бита указанного "агрегата". Возвращает количество
 произведённых сдвигов, или -1, если все биты "агрегата" равны 0 }
begin
  Result := NormShiftCount(P,Count);
  if Result > 0 then ShiftUpQWords(P,Count,Result);
end;

function SumOfQWords(XD,YD,SD: PQWordDigits; XP,YP,SP,XE,YE: Integer): Integer;
{ Возвращает в массиве SD ёмкостью SP "цифр" сумму чисел, представленных
  нормализованными массивами мантисс XD и YD ёмкостями соответственно
  XP и YP "цифр" и характеристиками соответственно XE и YE двоичных
  разрядов. Полученный массив есть нормализованная мантисса результата.
  Функция возвращает характеристику результата. }
var
  ExpDiff, QWordsShift, BitsShift: Integer;
  Carry: Boolean;
  Buffer: TCommonBuffer;
begin
  if XE < YE then
  begin
    SwapQWordPtrs(XD,YD);
    SwapInt(XP,YP);
    SwapInt(XE,YE);
  end;
  if XP > SP
  then CopyQWords(@Buffer,@XD[XP-SP-1],SP+1)
  else begin
    CopyQWords(@Buffer[SP-XP+1],XD,XP);
    ZeroQWords(@Buffer,SP-XP+1);
  end;
  Buffer[SP+1] := ZeroQWord;
  ExpDiff := XE-YE;
  QWordsShift := ExpDiff div 64;
  BitsShift := ExpDiff mod 64;
  ShiftUpQWords(@Buffer,SP+2,BitsShift);
  if QWordsShift <= SP
  then if YP+QWordsShift > SP
       then Carry := AddQWords(@Buffer,
                       @YD[YP+QWordsShift-SP-1],SP-QWordsShift+1) <> 0
       else Carry := AddQWords(@Buffer[SP-YP-QWordsShift+1],
                       YD,YP) <> 0
  else Carry := False;
  if Carry then IncQWords(@Buffer[SP-QWordsShift+1],QWordsShift+1);
  Inc(BitsShift,NormalizeQWords(@Buffer,SP+2));
  if Buffer[1].Hi < 0
  then if IncQWords(@Buffer[2],SP) <> 0 then
       begin
         Dec(BitsShift);
         Buffer[SP+1].Hi := MinDWord;
       end;
  CopyQWords(SD,@Buffer[2],SP);
  Result := XE + 64 - BitsShift;
end;

function DiffOfQWords(XD,YD,SD: PQWordDigits; XP,YP,SP,XE,YE: Integer;
           var Sign: TSign): Integer;
{ Возвращает в массиве SD ёмкостью SP "цифр" разность чисел, представленных
  нормализованными массивами мантисс XD и YD ёмкостями соответственно
  XP и YP "цифр" и характеристиками соответственно XE и YE двоичных
  разрядов. Полученный массив есть нормализованная мантисса результата.
  Функция возвращает характеристику результата.
  Sign - знак результата. }
var
  ExpDiff, MaxP, QWordsShift, BitsShift: Integer;
  Borrow: Boolean;
  Buffer: TCommonBuffer;
  Shift: Integer;
begin
  if XE < YE
  then Sign := Negative
  else if XE > YE
       then Sign := Positive
       else Sign := SignOfInt(CmpQWords(XD,YD,XP,YP));
  if Sign = Zero then
  begin
    ZeroQWords(SD,SP);
    Result := 0;
    Exit;
  end;
  if Sign = Negative then
  begin
    SwapQWordPtrs(XD,YD);
    SwapInt(XP,YP);
    SwapInt(XE,YE);
  end;
  MaxP := IntMax(SP,IntMax(XP,YP));
  CopyQWords(@Buffer[MaxP-XP+1],XD,XP);
  ZeroQWords(@Buffer,MaxP-XP+1);
  Buffer[MaxP+1] := ZeroQWord;
  ExpDiff := XE-YE;
  QWordsShift := ExpDiff div 64;
  BitsShift := ExpDiff mod 64;
  ShiftUpQWords(@Buffer,MaxP+2,BitsShift);
  if QWordsShift <= MaxP
  then if YP+QWordsShift > MaxP
       then Borrow := SubQWords(@Buffer,@YD[YP+QWordsShift-MaxP-1],
                        MaxP-QWordsShift+1) <> 0
       else Borrow := SubQWords(@Buffer[MaxP-YP-QWordsShift+1],YD,YP) <> 0
  else Borrow := False;
  if Borrow then DecQWords(@Buffer[MaxP-QWordsShift+1],QWordsShift+1);
  Shift := NormalizeQWords(@Buffer,MaxP+2);
  if Shift < 0 then raise ENum.Create(SInternal);
  Inc(BitsShift,Shift);
  if Buffer[MaxP-SP+1].Hi < 0
  then if IncQWords(@Buffer[MaxP-SP+2],SP) <> 0 then
       begin
         Dec(BitsShift);
         Buffer[MaxP+1].Hi := MinDWord;
       end;
  CopyQWords(SD,@Buffer[MaxP-SP+2],SP);
  Result := XE + 64 - BitsShift;
end;

function IncDecimals(P: PDecDigits; Count: Integer): Boolean;
var
  i: Integer;
begin
  i := Count - 1;
  repeat
    Result := P[i] = 9;
    if Result
    then P[i] := 0
    else Inc(P[i]);
    Dec(i);
  until (i<0) or not Result;
end;

function AllNines(P: PDecDigits; Count: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if P[i] <> 9 then Exit;
  Result := True;
end;

function AllBitsSet(P: PQWordDigits; Count: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if (P[i].Hi and P[i].Lo) <> -1 then Exit;
  Result := True;
end;

procedure SetBitsInDWord(var DW: DWord; FirstBit,LastBit: Integer);
{ 0 <= FirstBit, LastBit < 32 ! }
var
  FirstSHR, LastSHR, Mask: Integer;
begin
  if FirstBit < 0 then FirstBit := 0;
  if LastBit >= 32 then LastBit := 31;
  if FirstBit > LastBit then Exit;
  FirstSHR := (1 shl FirstBit) - 1;
  if LastBit = 31
  then LastSHR := -1
  else LastSHR := (1 shl (LastBit+1)) - 1;
  Mask := LastSHR - FirstSHR;
  DW := DW or Mask;
end;

procedure ResetBitsInDWord(var DW: DWord; FirstBit,LastBit: Integer);
var
  FirstSHR, LastSHR, Mask: Integer;
begin
  if FirstBit < 0 then FirstBit := 0;
  if LastBit >= 32 then LastBit := 31;
  if FirstBit > LastBit then Exit;
  FirstSHR := (1 shl FirstBit) - 1;
  if LastBit = 31
  then LastSHR := -1
  else LastSHR := (1 shl (LastBit+1)) - 1;
  Mask := not (LastSHR - FirstSHR);
  DW := DW and Mask;
end;

function AreAllBitsResetInDWord(var DW: DWord; FirstBit,LastBit: Integer): Boolean;
var
  FirstSHR, LastSHR, Mask: Integer;
begin
  Result := True;
  if FirstBit < 0 then FirstBit := 0;
  if LastBit >= 32 then LastBit := 31;
  if FirstBit > LastBit then Exit;
  FirstSHR := (1 shl FirstBit) - 1;
  if LastBit = 31
  then LastSHR := -1
  else LastSHR := (1 shl (LastBit+1)) - 1;
  Mask := LastSHR - FirstSHR;
  Result := (DW and Mask) = 0;
end;

procedure SetBitsInQWord(var QW: QWord; FirstBit,LastBit: Integer);
begin
  SetBitsInDWord(QW.Lo,FirstBit,LastBit);
  SetBitsInDWord(QW.Hi,FirstBit-32,LastBit-32);
end;

procedure ResetBitsInQWord(var QW: QWord; FirstBit,LastBit: Integer);
begin
  ResetBitsInDWord(QW.Lo,FirstBit,LastBit);
  ResetBitsInDWord(QW.Hi,FirstBit-32,LastBit-32);
end;

function AreAllBitsResetInQWord(var QW: QWord; FirstBit,LastBit: Integer): Boolean;
begin
  Result := AreAllBitsResetInDWord(QW.Lo,FirstBit,LastBit)
   and AreAllBitsResetInDWord(QW.Hi,FirstBit-32,LastBit-32);
end;

procedure SetBits(P: PQWordDigits; Count: Integer; FirstBit,LastBit: Integer);
var
  BitDiff,First,FirstRem,Last,LastRem: Integer;
begin
  if (Count <= 0) or (FirstBit > LastBit) or (FirstBit >= Count*64) then Exit;
  FirstBit := IntMax(0,FirstBit);
  LastBit := IntMin(Count*64-1,LastBit);
  BitDiff := LastBit - FirstBit;
  First := (FirstBit+63) div 64;
  Last := (LastBit+1) div 64 - 1;
  FirstRem := 64*First - FirstBit;
  LastRem := LastBit - 64*Last - 63;
  if Last - First >= 0 then FillQWords(@P[First],Last-First+1,MinusOneQWord);
  if FirstRem > 0
  then SetBitsInQWord(P[First-1],64-FirstRem,64-FirstRem+BitDiff);
  if LastRem > 0
  then SetBitsInQWord(P[Last+1],LastRem-BitDiff-1,LastRem-1);
end;

procedure ResetBits(P: PQWordDigits; Count: Integer; FirstBit,LastBit: Integer);
var
  BitDiff,First,FirstRem,Last,LastRem: Integer;
begin
  if (Count <= 0) or (FirstBit > LastBit) or (FirstBit >= Count*64) then Exit;
  FirstBit := IntMax(0,FirstBit);
  LastBit := IntMin(Count*64-1,LastBit);
  BitDiff := LastBit - FirstBit;
  First := (FirstBit+63) div 64;
  Last := (LastBit+1) div 64 - 1;
  FirstRem := 64*First - FirstBit;
  LastRem := LastBit - 64*Last - 63;
  if Last - First >= 0 then ZeroQWords(@P[First],Last-First+1);
  if FirstRem > 0
  then ResetBitsInQWord(P[First-1],64-FirstRem,64-FirstRem+BitDiff);
  if LastRem > 0
  then ResetBitsInQWord(P[Last+1],LastRem-BitDiff-1,LastRem-1);
end;

function AreAllBitsReset(P: PQWordDigits; Count: Integer; FirstBit,LastBit: Integer): Boolean;
var
  BitDiff,First,FirstRem,Last,LastRem: Integer;
begin
  Result := True;
  if (Count <= 0) or (FirstBit > LastBit) or (FirstBit >= Count*64) then Exit;
  FirstBit := IntMax(0,FirstBit);
  LastBit := IntMin(Count*64-1,LastBit);
  BitDiff := LastBit - FirstBit;
  First := (FirstBit+63) div 64;
  Last := (LastBit+1) div 64 - 1;
  FirstRem := 64*First - FirstBit;
  LastRem := LastBit - 64*Last - 63;
  if Last - First >= 0
  then begin
    if not AreQWordsZero(@P[First],Last-First+1)
    then begin
      Result := False;
      Exit;
    end;
  end;
  if FirstRem > 0
  then begin
    if not AreAllBitsResetInQWord(P[First-1],64-FirstRem,64-FirstRem+BitDiff)
    then begin
      Result := False;
      Exit;
    end;
  end;
  if LastRem > 0
  then Result := AreAllBitsResetInQWord(P[Last+1],LastRem-BitDiff-1,LastRem-1);
end;

function BitSet(P: PQWordDigits; Count,Number: Integer): Boolean;
asm
        shl     edx,6
        cmp     ecx,edx
        jae     @@Exit
        mov     edx,ecx
        and     edx,$1F
        shr     ecx,5
        bt      [eax+4*ecx],edx
@@Exit:
        setc    al
end;

{$IFDEF ARKRNL_RANGE} {$RANGECHECKS ON} {$ENDIF}
{$IFDEF ARKRNL_OVERFLOW} {$OVERFLOWCHECKS ON} {$ENDIF}

function Log2Up(C: Cardinal): Integer;
{ "Завышенный" двоичный логарифм числа }
begin
  Result := 0;
  while C > 0 do
  begin
    Inc(Result);
    C := C shr 1;
  end;
end;

function DecsToBits(DecPrec: Cardinal): Integer;
{ Вычисляет количество верных значащих бит в числе, в котором DecPrec
 верных десятичных значащих цифр. Более конкретно, возвращает "завышенное"
 значение двоичного логарифма числа, чей десятичный логарифм равен DecPrec }
begin
  Result := Trunc(DecPrec*Log_2_10)+2;
end;

function BitsToQWords(Bits: Cardinal): Integer;
{ Возвращает минимальное количество четверных слов, в которых содержится
 не менее Bits бит }
begin
  Result := (Bits-1) shr 6 + 1;
end;

function DecsToQWords(DecPrec: Cardinal): Integer;
{ Возвращает минимальное количество четверных слов, достаточных, чтобы
 обеспечить точность DecPrec десятичных цифр }
begin
  Result := BitsToQWords(DecsToBits(DecPrec));
end;

function BitsToDecs(BinPrec: Cardinal): Integer;
{ Возвращает минимальное количество десятичных разрядов, достаточных,
 чтобы обеспечить точность BinPrec двоичных разрядов }
begin
  Result := Trunc(BinPrec/Log_2_10+1);
end;

function QWordsToDecs(QWordPrec: Cardinal): Integer;
{ Возвращает минимальное количество десятичных разрядов, достаточных,
 чтобы обеспечить точность QWordPrec четверных слов }
begin
  Result := Trunc(QWordPrec/DecToQWord+1);
end;
