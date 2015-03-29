UNIT NewDelay;

{ Improved Delay routine
  For TP 6.0 and TP/BP 7.00/7.01 (real and protected mode).

  Version 1.00, released 29-09-97

  (Note: The code itself has NOT changed since version 0.96!
         I just thought it was about time to finish beta stage and make a
         version 1.00)

  Please send any BUG REPORTS, COMMENTS and other FEEDBACK to me:
  heckenb(at)mi.uni-erlangen.de or fjf(at)gmx.de

  Especially the accuracy probably still is to be improved a little. If
  someone wants to do that (using a high-precision timer - NOT the Bios system
  timer, or the GetTime procedure), please send me the results.

  The code is uncommented (as you will see yourself...), and I admit, some
  parts of it are probably not so easy to understand.
  If someone urgently needs some comments, I might write some.

 Copyright:
  Partially based on the Delay routine in the unit Crt,
   Copyright (C) 1988,91 Borland International.
  Any changes made are copyright 1996 by Frank Heckenbach; free for
  non-commercial use, only for people not affiliated with Microsoft in any way.
  You may change the code below for your own use (e.g. remove the "IFDEF"ed
  parts you don't need), but don't give away modified versions of it. If you
  think you made an improvement, please contact me at the address shown above.
  You may not remove or modify these comments from the beginning up to "***"!

 Disclaimer:
  This code is provided "as is", and comes WITHOUT ANY WARRANTY, expressed or
  implied; without even the implied warranty of fitness for a particular
  purpose. I shall not be liable for any damages, whether direct, indirect,
  special, incidental or consequential arising from the use of the software or
  from the inability to use the software or from a failure of the software to
  operate in a manner desired by the user.
  In short: Use this code totally at your own risk!

 Features:

  * DelayCnt changed from Word to Longint to avoid overflow on a 200+ MHz CPU.

  * No init code necessary (unlike Crt) - at least for BP 7.0, if you don't
    use runtime patching (see below). Delay is initialized when first called.
    This saves about 1/18s and some bytes in programs that don't use Delay.
    Thus, the first delay will be a bit inaccurate (+/- 1/36.4 s).
    You can call Delay(0) at the start of the program to initialize Delay
    and avoid this inaccuracy.

  * Tries to avoid busy waiting by giving up timeslices via INT $2F,$1680 when
    running on a 80386 or above in a multitasking environment (as Windoze
    or Linux). E.g., in Linux, the CPU usage during a Delay dropped from
    about 90% with the old Delay code to under 0.1% with this replacement.

    In such an environment, of course, the delay may get inaccurate,
    especially longer than intended (with the old code as well as with
    this replacement).

    Credits to Walter Koch (walterk@ddorf.rhein-ruhr.de) for pointing me to
    this interrupt function.

  * Can patch Crt at runtime and prevent a runtime error caused by Crt's init
    code on a fast CPU.

  * Tested under NWDOS 7.0, Windoze 3.1 and the Linux DosEmu by the author.

    Dan Dumbrill <73170.1423@CompuServe.COM> says:
    "... on a PPro200 with Windows 95 ...  the code seems to
     work fine with both protected and real modes."

    Michael Hermann <hermann@fmi.uni-passau.de> says:
    "I tried the fix, and it works also under OS/2 and Win95."

    Further feedback about the behaviour under OS/2 and Win95 is wanted.

 History:
  0.90 06-10-96 First release
  0.91 20-10-96 Minor improvement in Delay
  0.92 16-11-96 TP 6.0 compatibility added
  0.93 18-11-96 Crt patching added
  0.94 27-11-96 Added comments confirming OS/2 and Win95 compatibility
  0.95 00-00-00 Skipped this version number! :-)
  0.96 21-01-97 Added comments about using Make to modify Crt
  1.00 29-09-97 Added comments about Linux (DosEmu) compatibility
                Officially ended beta stage

 Bug (possibly):
  Ralf Brown's interrupt list says about INT $2F,$1680:
  'When called very often without intermediate screen output under WIN 3+,
   the VM will go into an idle-state and will not receive the next slice
   before 8 seconds. This time can be changed in SYSTEM.INI through
   "IdleVMWakeUpTime=<seconds>". Setting to zero results in a long wait.'
  However, I could not see this effect, so my routine does nothing to prevent
  this problem. If you encounter this problem, please contact me (address:
  see above).

 ***

 Using this unit together with Crt
 Choose one of the three solutions described below:

 FIRST solution
 + Easiest
 - Requires changing all your units and programs
 - Does not fix the 200 MHz problem

  Use NewDelay AFTER Crt in the uses clause ("Uses ...,Crt,NewDelay,...") of
  the main program and of ALL units that use Crt, otherwise Crt's Delay
  routine will be used instead of the new one.
  On a 200+ MHz CPU, Crt's init code related to Delay will produce a runtime
  error. Using this unit in this way won't help against that.

 SECOND solution
 + "Clean" solution
 + No programs or other units have to be modified
 + Other units don't even have to be recompiled
 - only for BP 7.0
 - Needs RTL source
 - Most work

  Modify Crt and rebuild your RTL. (Even if you are not afraid of the 200 MHz
  problem, it might be a good idea to change Crt, if you have the RTL source.)
  This is done as follows (Note: since Crt is copyrighted by Borland, I cannot
  distribute a modified version of it, nor will you be allowed to give away
  your modified version.):

  Preparations:
  * Read all of the following steps before you start. If there's anything you
    don't understand, don't start!
  * Of course you will make BACKUPS of any files you change during the
    following process (BEFORE you change anything)!
  * You must have BP7.0 with the RTL sources. If you only have TP 6.0 or
    TP 7.0, you can't use this solution.
  * Did I mention BACKUPS already?
  * You should have a bit more than basic experience working with BP, or have
    someone experienced to assist you in case of unexpected problems.
  * If you lose some important data without having made BACKUPS, you'll get
    some problems - so make BACKUPS NOW!

  Main part:
  * Remove all delay related parts from CRT.ASM (in the RTL\CRT directory).
    (Search for the string "delay" in the file, and keep your eyes open!
     Note: In the procedure "Initialize" it's the part from the line
     "MOV ES,Seg0040" to the line "MOV DelayCnt,AX", inclusively.)
  * Insert the implementation part of this unit - up to, but not including the
    line with "$IFDEF PATCHCRT" - into the implementation of CRT.PAS (same
    directory), and remove the line "procedure Delay; external;" from it.
    Don't change anything in the interface part of CRT.PAS.
  * Instead of the next two or three steps, you can change into the RTL
    directory and call "make \bp\rtl\bin\tpu\crt.tpu" and
    "make \bp\rtl\bin\tpu\crt.tpp" or simply "make", respectively.
    However, this may not work if your directories aren't set up exactly as
    the makefile expects them to.
  * Assemble CRT.ASM (with -d_DPMI_ to CRT.OBP for protected mode, and without
    this switch to CRT.OBJ for real mode) with TASM.
  * Compile CRT.PAS to CRT.TPU and CRT.TPP with BPC.
  * Update CRT.TPU and CRT.TPP in TURBO.TPL and TPP.TPL, respectively, with
    TPUMOVER (or, alternatively: remove them from TURBO/TPP.TPL and include
    the path to either CRT.PAS or CRT.TP? into your unit directories, and in
    the former case also the path to CRT.OB? into your object directories).
  * After modifying Crt this way, you don't have to use NewDelay in your
    programs, of course.

 THIRD solution
 + Easy
 + No RTL source needed
 + Only the program - no other units - has to be modified and recompiled
 - Kind of a workaround
 - Not for protected mode

  This method patches Crt at runtime, i.e. the code in the Crt unit is
  modified whenever a program compiled with this unit is started. However,
  Crt's Delay procedure is not really fixed, just "redirected" to this unit's
  Delay procedure. Therefore, two versions of Delay will exist in the
  executable file, making it bigger than actually necessary.
  Additionally, an interrupt handler is installed to trap the "division by
  zero" error caused by Crt's init code. This works only for real mode.
  However, Windoze does not have Crt at all (and WinCrt does not have Delay),
  and protected mode is only available with BP 7.0 which comes with the RTL
  source, so you can use the second solution in this case. It should be
  obvious that installing a (temporary) interrupt handler is also not a very
  "clean" solution, and makes the executable bigger than necessary, but anyway
  it works.

  How to do it:
  * Define the symbol PATCHCRT in this unit, i.e. remove the # in the
    following line:
    }

    {$DEFINE PATCHCRT}

    {
  * Use NewDelay IMMEDIATELY BEFORE Crt, and before any units that use Crt in
    the uses clause of the main program ("Uses NewDelay,Crt...")
  * Insert the following line at the start of the main program:
    PatchCrt(Crt.Delay);
}

{$IFDEF WINDOWS}
This unit is not for Windoze!
{$ENDIF}

INTERFACE

{$IFDEF PATCHCRT}
USES {$IFDEF WINDOWS}WinProcs{$ELSE}Dos{$ENDIF};

TYPE TCrtDelay=PROCEDURE(ms:Word);
PROCEDURE PatchCrt(CrtDelay:TCrtDelay);
{$ENDIF}

{$IFDEF VER60}
CONST
  Seg0040:Word=$40;
  Test8086:Byte=0; {Will be set to 2 if processor is 80386 or above}
{$ENDIF}

PROCEDURE Delay(ms:Word);

IMPLEMENTATION

CONST TimeSlice=100; {Threshold (in ms), above which Delay tries to give up
                      time slices. Can be changed.}

PROCEDURE DelayLoop; NEAR; ASSEMBLER; {Internal!}
ASM
@1:SUB  AX,1
   SBB  DX,0
   JC   @2
   CMP  BL,ES:[DI]
   JE   @1
@2:
END;

PROCEDURE Delay(ms:Word); ASSEMBLER;
TYPE LongRec=RECORD Lo,Hi:Word END;
CONST DelayCnt:Longint=0; {0 means unitialized}
CONST op32=$66; {Prefix for 32bit operations}
ASM
   MOV  ES,Seg0040
   MOV  CX,ms
   MOV  SI,$6C
   MOV  AX,DelayCnt.LongRec.Lo
   OR   AX,DelayCnt.LongRec.Hi
   JNE  @2
   MOV  DI,SI
   MOV  BL,ES:[DI]
@1:CMP  BL,ES:[DI]
   JE   @1
   MOV  BL,ES:[DI]
   MOV  AX,-28
   CWD
   CALL DelayLoop
   NOT  AX
   NOT  DX
   MOV  BX,AX
   MOV  AX,DX
   XOR  DX,DX
   MOV  CX,55
   DIV  CX
   MOV  DelayCnt.LongRec.Hi,AX
   MOV  AX,BX
   DIV  CX
   MOV  DelayCnt.LongRec.Lo,AX
   MOV  CX,ms
   SUB  CX,83
   JBE  @x
@2:JCXZ @x
   XOR  DI,DI
   MOV  BL,ES:[DI]
   CMP  Test8086,2
   JNB  @4
@3:XOR  SI,SI
@4:MOV  BH,ES:[SI]
@5:MOV  AX,DelayCnt.LongRec.Lo
   MOV  DX,DelayCnt.LongRec.Hi
   CALL DelayLoop
   CMP  BH,ES:[SI]
   JNE  @7
@6:LOOP @5
   JMP  @x
@7:CMP  CX,TimeSlice
   JB   @6
   DB   op32;MOV DX,ES:[SI]
@8:MOV  AX,$1680
   INT  $2F
   OR   AL,AL
   JNZ  @3
   DB   op32;MOV AX,DX
   DB   op32;MOV DX,ES:[SI]
   DB   op32;SUB AX,DX
   JBE  @9
   DB   op32;MOV AX,DX
   JMP  @a
@9:DB   op32;NEG AX
@a:DB   op32;CMP AX,$4A7;DW 0 {CMP EAX,$10000 DIV 55}
   JA   @x
   PUSH DX
   PUSH CX
   MOV  CX,55
   MUL  CX
   POP  CX
   POP  DX
   SUB  CX,AX
   JBE  @x
   CMP  CX,TimeSlice
   JNB  @8
   JMP  @3
@x:
END;

{$IFDEF PATCHCRT}
PROCEDURE Patch(OldProc,NewProc:Pointer);
{General patch procedure.
 Patch writes a far jump to NewProc at the beginning of OldProc, thus
 directing all calls to OldProc to NewProc.
 OldProc and NewProc must both be pointers to far procedures/functions with
 the same number, order and type of parameters and the same return type (if
 functions). If they are different, no immediate error is generated, but most
 likely the program will crash when OldProc is called.
 Should also work with procedures/functions in overlaid units.}
TYPE
  TFarJmp=RECORD
    OpCode:Byte;
    Operand:Pointer
  END;
BEGIN
  {Get a writeable pointer to OldProc}
  {$IFDEF DPMI}
  OldProc:=Ptr(Seg(OldProc^)+SelectorInc,Ofs(OldProc^));
  {$ENDIF}
  {$IFDEF WINDOWS}
  OldProc:=Ptr(AllocCStoDSAlias(Seg(OldProc^)),Ofs(OldProc^));
  {$ENDIF}
  WITH TFarJmp(OldProc^) DO
    BEGIN
      OpCode:=$EA; {JMP FAR PTR}
      Operand:=NewProc
    END;
  {$IFDEF WINDOWS}
  FreeSelector(Seg(OldProc^))
  {$ENDIF}
END;

{$IFDEF MSDOS}
CONST OldInt0P:Pointer=NIL;
VAR OldInt0:PROCEDURE(Flags:Word) ABSOLUTE OldInt0P;
{"CONST OldInt0:PROCEDURE(Flags:Word)=NIL" is not possible in TP 6.0!}
{$ENDIF}

PROCEDURE PatchCrt(CrtDelay:TCrtDelay);
BEGIN
  Patch(@CrtDelay,@Delay);
  {$IFDEF MSDOS}
  IF @OldInt0<>NIL THEN SetIntVec(0,@OldInt0) {No init bug has occurred!}
  {$ENDIF}
END;

{$IFDEF MSDOS}

PROCEDURE NewInt0(Flags,CS,IP,AX,BX,CX,DX,SI,DI,DS,ES,BP:Word); INTERRUPT;
BEGIN
  IF MemW[CS:IP]=$F1F7 {DIV CX}
     {Not a foolproof check, but should be sufficient, since NewDelay should
      be used IMMEDIATELY before Crt}
    THEN
      BEGIN
        Writeln('Crt init bug trapped!');
        SetIntVec(0,@OldInt0);
        @OldInt0:=NIL;
        DX:=CX-1
      END
    ELSE OldInt0(Flags)
END;

BEGIN
  GetIntVec(0,@OldInt0);
  SetIntVec(0,@NewInt0);
{$ENDIF}
{$ENDIF}

{$IFDEF VER60}
BEGIN
ASM {Check for 80386}
   PUSHF
   POP  AX
   OR   AH,$F0
   PUSH AX
   POPF
   PUSHF
   POP  AX
   AND  AH,$F0
   JE   @1
   MOV  Test8086,2
@1:
END
{$IFDEF PATCHCRT}
{$IFDEF MSDOS}
END
{$ENDIF}
{$ENDIF}
{$ENDIF}
END.
