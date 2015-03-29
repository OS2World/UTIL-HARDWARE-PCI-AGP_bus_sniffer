(* option to emulate a different machine using pirt.dat and pci.log *)
{- $Define pci_Debug}

{&Use32+}
{$G+}
{$R+}
{$S-}
{$I+}
{$N-}

unit pci_ca; {cache+helper layer above pci_hw}

interface

procedure reset_infotbl_cache;
procedure set_lookup_device(const deviceid,func,bus:word);
function  infotbl  (const i:word):byte;
function  infotbl_W(const i:word):Word;
function  infotbl_L(const i:word):longint;
function  headertype:byte;
function  is_multifunction:boolean;
function  first_cap_ptr:byte;
function  cap_ptr_valid(const cap_ptr:byte):boolean;
function  is_bridge_with_pass_pci_irq:boolean;

var
  infotbl_deviceid,
  infotbl_func,
  infotbl_bus   : word;

implementation

uses
  pci_hw;

var
  infotbl_cache : array[0..$ff] of byte;
  infotbl_read  : array[0..$ff] of boolean;

(* Read-Cache functions for PCI register space
   replaces old array infotbl - no need to read ahead and hang.. *)

procedure reset_infotbl_cache;
  begin
    FillChar(infotbl_read,SizeOf(infotbl_read),false);
  end;

procedure set_lookup_device(const deviceid,func,bus:word);
  begin
    infotbl_deviceid:=deviceid;
    infotbl_func    :=func;
    infotbl_bus     :=bus;
    reset_infotbl_cache;
  end;

{$IfNDef VirtualPascal}
procedure SetLength(var S: String; NewLength: integer);
  begin
    s[0]:=Chr(NewLength);
  end;
{$EndIf}

function Int2Str(const l:longint):string;
  {$IfNDef VirtualPascal}
  var
    Result              :String;
  {$EndIf}
  begin
    Str(l,Result);
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

function infotbl(const i:word):byte;
  {$IfDef pci_Debug}
  var
    f                   :text;
    s1,s2,z             :string;
    zi,zj               :word;
    code                :integer;
    buffer              :array[0..{32*1024-1}8*1024-1] of byte;
  {$EndIf pci_Debug}
  begin
    {$IfDef pci_Debug}
    if not infotbl_read[i] then
      begin
        FillChar(infotbl_cache,SizeOf(infotbl_cache),$ff);

        Assign(f,'pci.log');
        {$I-}
        Reset(f);
        {$I+}
        if IOResult<>0 then
          begin
            WriteLn('Can load pci.log replay file.');
            WriteLn('Solution: recompile without Debug_ROM;pci_Debug in Options/Compiler');
            Halt(1);
          end;

        SetTextBuf(f,buffer,SizeOf(buffer));

        s1:=' Bus '+Int2Str(infotbl_bus)+' (';
        s2:='), Device Number '+Int2Str(infotbl_deviceid)+', Device Function '+Int2Str(infotbl_func);
        while not Eof(f) do
          begin
            ReadLn(f,z);
            if (Pos(s1,z)=1) and (Pos(s2,z)>0) then
              begin
                while (not Eof(f)) and (z<>' Hex-Dump of device configuration space follows:') do
                  ReadLn(f,z);

                for zi:=$0 to $f do
                  begin
                    s1:='  '+Int2Hex(zi shl 4,4)+'  ';
                    if Eof(f) then Break;
                    ReadLn(f,z);
                    if Pos(s1,z)=1 then
                      begin
                        Delete(z,1,Length(s1));
                        for zj:=$0 to $f do
                          begin
                            Val('$'+Copy(z,1,2),infotbl_cache[zi shl 4+zj],code);
                            Delete(z,1,3);
                          end;
                      end;
                  end;
                Break;
              end;
          end;
        Close(f);
        FillChar(infotbl_read,SizeOf(infotbl_read),true);
      end;
    {$Else pci_Debug}
    if not infotbl_read[i] then
      begin
        { old code: read every byte in an single operation
        infotbl_cache[i]:=lookup(infotbl_deviceid,infotbl_func,infotbl_bus,i);
        infotbl_read [i]:=true;
          new code: just use full 32 bits read from port CF8/CFC.. }
        pLongint(@infotbl_cache[i and $fc])^:=
          lookup_dword(infotbl_deviceid,infotbl_func,infotbl_bus,i and $fc);
        FillChar(infotbl_read[i and $fc],4,true);
      end;
    {$EndIf pci_Debug}
    infotbl:=infotbl_cache[i];
  end;

function infotbl_W(const i:word):Word;
  begin
    infotbl_W:=Word(infotbl(i+1)) shl 8
              +     infotbl(i  )       ;
  end;

function infotbl_L(const i:word):longint;
  begin
    infotbl_L:=Longint(infotbl_W(i+2)) shl 16
              +        infotbl_W(i  )        ;
  end;

function headertype:byte;
  begin
    headertype:=infotbl($0e) and $7f; {usually 0,1,2}
  end;

function is_multifunction:boolean;
  begin
    if infotbl_func>0 then (* the multifunction bit is only valid for funtion 0 *)
      is_multifunction:=true
    else (* if there is no device at function 0, then ther is no device *)
      if infotbl_W($00)=$ffff then
        is_multifunction:=false
      else (* else the multifunction bit (header type or $80) is valid *)
        is_multifunction:=Odd(infotbl($0e) shr 7);
  end;

function first_cap_ptr:byte;
  begin
    first_cap_ptr:=0;
    (* status register: new capabilities list is present? *)
    if Odd(infotbl($06) shr 4) then
      case headertype of
        0:first_cap_ptr:=infotbl($34);
        1:first_cap_ptr:=infotbl($34);
        2:first_cap_ptr:=infotbl($14);
      end;
  end;

function cap_ptr_valid(const cap_ptr:byte):boolean;
  begin
    (* must be >40, aligned to dword.
       this code does not checks for loops. *)
    cap_ptr_valid:=(cap_ptr>=$40) and (cap_ptr<=$fc) and (cap_ptr and 3=0);
  end;

function is_bridge_with_pass_pci_irq:boolean;
  begin
    case headertype of
      1:is_bridge_with_pass_pci_irq:=true;
      2:is_bridge_with_pass_pci_irq:=not Odd(infotbl_W($3e) shr 7);
        (* 1: ints routed by ExCA *)
        (* 0: ints routed by PCI  *)
    else
        is_bridge_with_pass_pci_irq:=false;
    end;
  end;

end.

