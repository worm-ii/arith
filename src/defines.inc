{ 
  arith - arbitrary precision floaing point library in Delphi.
  Include file for distinguishing Delphi 1, Delphi 2 and Delphi 3 from higher versions
}
{$IFNDEF VER80}
  {$IFNDEF VER90}
    {$DEFINE D3PLUS}
    {$IFNDEF VER100}
      {$IFNDEF VER110}
        {$DEFINE D4PLUS}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFOPT R+} {$DEFINE ARKRNL_RANGE} {$ENDIF}
{$IFOPT Q+} {$DEFINE ARKRNL_OVERFLOW} {$ENDIF}
