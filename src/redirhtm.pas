(* paging functions *)

{&Use32+}
{$G+}
{$R+}
{$S-}
{$I+}
{$N-}

unit RedirHtm;

interface

procedure install_html_ouput(const title:string);
procedure begin_body_html;
procedure write_extra_html(const s:string);
procedure writeln_extra_html(const s:string);

implementation

uses
  Crt,
  Dos;

const
  pal16:array[0..15] of record r,g,b:byte end=(
    (r:  0;g:  0;b:  0), (* could use color names, but they are not 'exact' matches *)
    (r:  0;g:  0;b:170),
    (r:  0;g:170;b:  0),
    (r:  0;g:170;b:170),
    (r:170;g:  0;b:  0),
    (r:170;g:  0;b:170),
    (r:170;g: 85;b:  0),
    (r:170;g:170;b:170),
    (r: 85;g: 85;b: 85),
    (r: 85;g: 85;b:255),
    (r: 85;g:255;b: 85),
    (r: 85;g:255;b:255),
    (r:255;g: 85;b: 85),
    (r:255;g: 85;b:255),
    (r:255;g:255;b: 85),
    (r:255;g:255;b:255));

var
  Output_org            :Text;

const
  org_FlushFunc         :Pointer=nil;
  org_InOutFunc         :Pointer=nil;
  org_CloseFunc         :Pointer=nil;
  chars_on_line         :word=0;
  last_TextAttr         :byte=$07;

{$IfNDef VirtualPascal}
procedure SetLength(var S: String; NewLength: integer);
  begin
    s[0]:=Chr(NewLength);
  end;
{$EndIf}

function Int2Str(const i:longint):string;
  {$IfNDef VirtualPascal}
  var
    Result              :String;
  {$EndIf}
  begin
    Str(i,Result);
    {$IfNDef VirtualPascal}
    Int2Str:=Result;
    {$EndIf}
  end;

function Int2Hex(const l:longint;const n:word):string;
  const
    hexdigit            :array[$0..$f] of char='0123456789ABCDEF';
  var
    {$IfNDef VirtualPascal}
    Result              :String;
    {$EndIf}
    i                   :word;
  begin
    SetLength(Result,n);
    for i:=0 to n-1 do
      Result[n-i]:=hexdigit[(l shr (4*i)) and $f];
    {$IfNDef VirtualPascal}
    Int2Hex:=Result;
    {$EndIf}
  end;


function color_rgb(const a:byte):string;
  begin
    with pal16[a] do
      color_rgb:='rgb('+Int2Str(r)+', '+Int2Str(g)+', '+Int2Str(b)+')';
  end;

(* add html line begin/endings  *)
(* add color encoding           *)
(* convert special chars        *)
function html_write(var t:Text):longint;far;{&Saves ebx,ecx,edx,esi,edi}
  var
    i                   :integer;
  begin
    {$I-}
    html_write:=0;
    with TextRec(t) do
      begin
        if BufPos=0 then Exit;
        for i:=0 to BufPos-1 do
          begin

            (* new line *)
            {if chars_on_line=0 then
              Write(Output_org,'');}

            Inc(chars_on_line);

            (* color change *)
            if TextAttr<>last_TextAttr then
              begin
                if last_TextAttr<>$07 then
                  begin
                    Write(Output_org,'</span>');
                    last_TextAttr:=$07;
                  end;
                if TextAttr<>$07 then
                  begin
                    Write(Output_org,'<span class=A0'+Int2Hex(TextAttr and $0f,1)+'>');
                    last_TextAttr:=TextAttr;
                  end;
              end;

            case BufPtr^[i] of
              #10:
                begin
                  Dec(chars_on_line); (* control char *)

                  (* end different color *)
                  if TextAttr<>$07 then
                    begin
                      Write(Output_org,'</span>');
                      last_TextAttr:=$07;
                    end;

                  (* end line *)
                  if chars_on_line=0 then
                    Write(Output_org,'&nbsp;'); (* lynx does not render blank lines *)

                  WriteLn(Output_org,'<br>');
                  chars_on_line:=0;
                end;
              #13:
                begin
                  Dec(chars_on_line); (* control char *)
                end;
            else

              case BufPtr^[i] of
                '<':Write(Output_org,'&lt;');
                '>':Write(Output_org,'&gt;');
                '&':Write(Output_org,'&amp;');
                ' ':Write(Output_org,'&nbsp;');
                '"':Write(Output_org,'&quot;');

                (* only valid/needed in XML/XHTML:
               '''':Write(Output_org,'&apos;'); *)

                (* chars are valid for IBM850, but not for Netscape 4.61.. *)
                'Ä':Write(Output_org,'&#9472;'); (* 2500 *)
                'À':Write(Output_org,'&#9492;'); (* 2514 *)
                'Ã':Write(Output_org,'&#9500;'); (* 251c *)
                '³':Write(Output_org,'&#9474;'); (* 2502 *)
                (* pity that the do not look better than > and < *)
               #$10:Write(Output_org,'&#9658;'); (* 25ba *)
               #$11:Write(Output_org,'&#9668;'); (* 25c4 *)
              else
                    Write(Output_org,BufPtr^[i]);
              end;
              Inc(chars_on_line);
            end;

          end; (* i *)

        BufPos:=0;
      end;
    {$I+}
    html_write:=IOResult;
  end;

procedure close_html_output(var t:Text);
  begin
    {$I-}
    Close(Output_org);
    with TextRec(Output) do
      begin
        FlushFunc:=org_FlushFunc;
        InOutFunc:=org_InOutFunc;
        CloseFunc:=org_CloseFunc;
      end;
    WriteLn('</div></body>');
    WriteLn('</html>');
    {$I+}
  end;

function CloseFunc_filter(var t:Text):longint;far;assembler;
  {&Uses ebx}{&Frame-}
  asm
    {$IfDef VirtualPascal}
    mov ebx,[t]
    push ebx
    call close_html_output
    push ebx
    call TextRec[ebx].CloseFunc
    {$Else}
    les bx,[t]
    push es
    push bx
    call close_html_output
    les bx,[t]
    push es
    push bx
    call TextRec[es:bx].CloseFunc
    {$EndIf}
  end;


procedure install_html_ouput(const title:string);
  var
    i                   :word;
  begin
    Assign(Output_org,'');
    Rewrite(Output_org);
    WriteLn(Output_org,'<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN">');
    WriteLn(Output_org,'<html>');
    WriteLn(Output_org,'<head>');
    WriteLn(Output_org,'<meta http-equiv="content-type" content="text/html; charset=IBM850">');
    WriteLn(Output_org,'<meta name="generator" content="redirhtm.pas">');

    WriteLn(Output_org,'<title>',title,'</title>');

    WriteLn(Output_org,'<style type="text/css" media=screen>');
    WriteLn(Output_org,'<!--');
    WriteLn(Output_org,'  BODY { font-family: monospace; color: '+color_rgb(7)+'; background-color: '+color_rgb(0)+' }');

    for i:=0 to 15 do
      WriteLn(Output_org,'  .A0'+Int2Hex(i,1)+' { color: '+color_rgb(i)+' }');

    WriteLn(Output_org,'  a:link    { text-decoration: none;      color: inherit }');
    WriteLn(Output_org,'  a:visited { text-decoration: none;      color: inherit }');
    WriteLn(Output_org,'  a:hover   { text-decoration: none;      color: inherit }');
    WriteLn(Output_org,'  a:active  { text-decoration: underline; color: inherit }');
    WriteLn(Output_org,'  a:focus   { text-decoration: underline; color: inherit }');
    WriteLn(Output_org,'  -->');
    WriteLn(Output_org,'</style>');

    WriteLn(Output_org,'<style type="text/css" media=print>');
    WriteLn(Output_org,'<!--');
    WriteLn(Output_org,'  BODY { font-family: monospace; color: black; background-color: white }');

    { 0.. 6,8 }
    WriteLn(Output_org,'  .A00, .A01, .A02, .A03, .A04, .A05, .A06, .A08 { color: gray }');
    { 7 }
    WriteLn(Output_org,'  .A07 {  }');
    { 9..15 }
    WriteLn(Output_org,'  .A09, .A0A, .A0B, .A0C, .A0D, .A0E, .A0F { font-weight:bold }');

    WriteLn(Output_org,'  a:link, a:visited, a:hover, a:active, a:focus { text-decoration: none; color: inherit}');
    WriteLn(Output_org,'  -->');
    WriteLn(Output_org,'</style>');

    with TextRec(Output) do
      begin
        org_FlushFunc:=FlushFunc;
        FlushFunc:=@html_write;
        org_InOutFunc:=InOutFunc;
        InOutFunc:=@html_write;
        org_CloseFunc:=CloseFunc;
        CloseFunc:=@CloseFunc_filter;
      end;
  end;

procedure begin_body_html;
  begin
    WriteLn(Output_org,'</head>');
    WriteLn(Output_org,'<body><div>');
  end;

procedure write_extra_html(const s:string);
  begin
    { flush color changes, so that <span> </span> will not cross <a href> </a> }
    if (TextAttr=$07) and (last_TextAttr<>TextAttr) then
      begin
        Write(Output_org,'</span>');
        last_TextAttr:=$07;
      end;

    Write(Output_org,s);
  end;

procedure writeln_extra_html(const s:string);
  begin
    { flush color changes, so that <span> </span> will not cross <a href> </a> }
    if (TextAttr=$07) and (last_TextAttr<>TextAttr) then
      begin
        Write(Output_org,'</span>');
        last_TextAttr:=$07;
      end;

    WriteLn(Output_org,s);
  end;

end.

