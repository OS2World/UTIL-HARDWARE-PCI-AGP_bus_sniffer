(* paging functions *)

{&Use32+}
{$G+}
{$R+}
{$S-}
{$I+}
{$N-}

unit RedirCon;

interface

const
  terminate_request:boolean=false;

procedure install_pager;
function  ioredirected:boolean;
{$IfNDef VirtualPascal}
procedure SetLength(var s:string;const l:word);
function Max(const b1,b2:byte):byte;
{$EndIf}

implementation

uses
  {$IfDef VirtualPascal}
  VpSysLow,
  VpUtils,
  {$EndIf}
  Crt,
  Dos;

var
  linecounter           : word;
  org_FlushFunc         : pointer;

const
  linecounter_att       :byte=$1e;
  linecounter_str       :string[16]='Press a key ... ';
  linecounter_del       :string[16]='                ';

{$IfNDef VirtualPascal}
procedure SysWrtStrAtt(const s:string;const attr:byte);
  var
    p                   :^byte;
    i                   :word;
  begin

    if Mem[Seg0040:$49]=7 then
      p:=Ptr(SegB000,(WhereY-1)*Mem[Seg0040:$4a]*2)
    else
      p:=Ptr(SegB800,(WhereY-1)*Mem[Seg0040:$4a]*2);

    for i:=1 to Length(s) do
      begin
        p^:=Ord(s[i]);Inc(p);
        p^:=attr     ;Inc(p);
      end;

  end;
{$EndIf VirtualPascal}

{$IfDef VirtualPascal}
(* counts lines *)
procedure pagefilter1(var t:Text);{&Saves All}
  var
     z                  :word;
  begin
    with TextRec(t) do
      if BufPos>0 then
        for z:=1-1 to BufPos-1 do
          if BufPtr^[z]=#10 then
            Inc(linecounter);
  end;
{$Else VirtualPascal}
procedure pagefilter1(var t:Text);assembler;
  asm
    push ax
    push di
    push es
    push cx

    les di,[t]
    mov cx,es:[di+TextRec.BufPos]
    les di,es:[di+TextRec.BufPtr]
    cld
    mov al,10

  @sl:
    jcxz @ret
    dec cx
    scasb
    jne @sl
    inc linecounter
    jmp @sl

  @ret:
    pop cx
    pop es
    pop di
    pop ax
  end;
{$EndIf VirtualPascal}


(* displays prompt, if line counter reaches screen size *)
{$IfDef VirtualPascal}
procedure pagefilter2;{&Saves All}
  begin
    if (linecounter>=Hi(WindMax)) and (WhereX=1) then
      begin
        SysWrtCharStrAtt(@linecounter_str[1],Length(linecounter_str),0,WhereY-1,linecounter_att);
        {$IfNDef Debug}
        case SysReadKey of
          #$00,#$e0:
            case SysReadKey of
              #$2d: (* Alt+X *)
                terminate_request:=true;
            end;
          #27: (* Esc *)
             terminate_request:=true;
          'Q','q':
             terminate_request:=true;
        end;
        {$EndIf}
        SysWrtCharStrAtt(@linecounter_del[1],Length(linecounter_del),0,WhereY-1,TextAttr);
        linecounter:=0;
      end;
  end;
{$Else VirtualPascal}
procedure pagefilter2;assembler;
  asm
    push ax
    mov ax,WindMax
    shr ax,8
    cmp linecounter,ax
    jb @ret

    pusha
    push es


      push ds
      push Offset linecounter_str
      mov ah,0
      mov al,linecounter_att
      push ax
      call SysWrtStrAtt

      sub ax,ax
      int $16
      cmp al,'q'
      je @terminate_req
      cmp al,'Q'
      je @terminate_req
      cmp al,27
      je @terminate_req
      cmp ax,$2d00
      jne @no_terminate_req
  @terminate_req:
      mov terminate_request,true
  @no_terminate_req:
      mov linecounter,0

      push ds
      push Offset linecounter_del
      mov ah,0
      mov al,TextAttr
      push ax
      call SysWrtStrAtt

    pop es
    popa

  @ret:
    pop ax
  end;
{$EndIf VirtualPascal}


(* call line counter, write output, call prompt display *)
function FlushFunc_page_output(var t:Text):integer;far;assembler;
  {&Uses ebx}{&Frame-}
  asm
    {$IfDef VirtualPascal}
    mov ebx,[t]
    push ebx
    {$Else}
    les bx,[t]
    push es
    push bx
    {$EndIf}
    call pagefilter1

    {$IfDef VirtualPascal}
    push ebx
    {$Else}
    les bx,[t]
    push es
    push bx
    {$EndIf}
    call [org_FlushFunc]

    call pagefilter2
  end;


procedure install_pager;
  begin
    linecounter:=0;
    with TextRec(Output) do
      begin
        org_FlushFunc:=FlushFunc;
        FlushFunc:=@FlushFunc_page_output;
      end;
  end;

{$IfDef VirtualPascal}
function IORedirected : boolean ;
  begin
    IORedirected:=not VPUtils.IsFileHandleConsole(SysFileStdOut);
  end;
{$Else}
function IORedirected : boolean ; Assembler;
  asm
    mov es,PrefixSeg
    xor bx,bx
    {$IfDef DPMI}
    push Word Ptr es:[bx+ $34+2]
    mov ax,$0002              (* DPMI: segment->descriptor *)
    mov bx,es:[bx+ $34+0]
    int $31
    mov es,ax
    pop bx
    {$Else}
    les bx,es:[bx + $34]
    {$EndIf}
    mov al,es:[bx]
    mov ah,es:[bx +1]
    cmp al,ah
    mov al,true
    jne @exit

    mov al,false

   @exit:
  end;
{$EndIf}


{$IfNDef VirtualPascal}
procedure SetLength(var s:string;const l:word);
  begin
    s[0]:=Chr(l);
  end;
function Max(const b1,b2:byte):byte;
  begin
    if b1>b2 then
      Max:=b1
    else
      Max:=b2;
  end;
{$EndIf}


end.
