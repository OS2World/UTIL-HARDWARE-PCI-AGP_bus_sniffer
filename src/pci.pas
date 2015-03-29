
{-$Define AskPowerChange} { special test code }
{$Define show_devicenames_tree} { use database lookup - fast enough when pcidevs.txt is buffered, a bit more output }
{-$Define show_classcode_tree} { this is what PCI32 uses - just display VVVV:DDDD classcode string }

{$IFDEF VirtualPascal}
  {&Use32+}
  {&Delphi-}
  {&AlignCode+}
  {&AlignData+}
  {&AlignRec-}
  {&Optimize+}
  {$M 40000}
{$ELSE}
  {$M 30000,0,0}
{$ENDIF}


Program PCI;

{$B-}
{$G+}
{$R+}{ Craig uses R- since 1.01? }
{$S-}
{$I+}
{$N-}
{$E-}

{$IfDef OS2}
  {$Define Linux_OS2_Win32}
  {-$R os2.res}
{$EndIf}

{$IfDef Win32}
  {$Define Linux_OS2_Win32}
  {$R win32.res}
{$EndIf}

{$IfDef Linux}
  {$Define Linux_OS2_Win32}
{$EndIf}


uses
  {$IfDef VirtualPascal}
  {$IfDef Debug} callstck, {$EndIf}
  VpUtils,
  VpSysLow,
  {$Else}
  {newdelay, - we do not Delay, and use a update RTL for DOS real AND DPMI/16}
  {$EndIf}
  WinDos,Dos,
  Crt,
  Strings,
  pci_hw,
  pci_ca,
  RedirCon,
  RedirHtm,
  RmResSz;


{$I classes.pas}


{
  This code is Written by Craig Hart in 1996-2005. It is released as freeware;
  please use and modify at will. No guarantees are made or implied.


  Please read the accompaning documentation PCI.DOC, readmew.txt and
  pci101vk.txt for all the info relating to this program!
}



const
  revision      ='1.04vkA'; { when you change the source and update the version number,
                              please do not forget to als change:
                                - pci.def (executable bldlevel string)
                                - file_id.diz file
                                - rename/enhance pcixxxvk.txt documentation
                                - _.cmd build batch target archive filename }

  FModeReadOnly = $40; { open_share_DenyNone+open_access_ReadOnly }

  chars_to_     ='..';
 {chars_to_     =' to ';}

type
  link_use_type = set of byte;
  linkuse_array_type = array[0..255] of link_use_type;

  treedata      =
    record
      b,                {bus/device/function}
      d,
      f         : byte;
      vid,              {vendor/device code found}
      did       : word;
    end;

  bustype_types = (bustype_PCI_known,
                   bustype_PCI,
                   bustype_AGP,
                   bustype_CardBus,
                   bustype_PCI_E,
                   bustype_PCI_X,
                   bustype_None);

var
  apic,                 { int >=16 found }
  businfo,              { -B parameter } { obsoleted }
  dorouting,            { -T parameter }
  dopcirouting,         { -P parameter }
  summary,              { -S parameter }
  dumpregs,             { -D parameter }
  first,                { printstatus: first write }
  installermode,        { -I parameter }
  showhelp,             { -H parameter }
  readonly,             { -N (R) parameter }
  showtree,             { -R paramter }
  vga50,                { -5 parameter }
  force7bittree,        { -7 parameter }
  dohtml                { -M parameter }
                : boolean;


  irqmap        : array[byte] of byte; { usage count for irqs }

  f             : text; { pcidevs.txt }

  cmdstr,
  revchk,
  vstr,
  cmpstr        : string;

  pcidevs_txt   : string;
  pcidevs_path  : string;

  bustype       : array[0..255] of bustype_types;               { Identified type of each bus }

  { false means parent bridge known, or parent bridge='CPU',
    true  means that the bus is only secondary+1..subordinate,
    without actualy existing}
  behind_secondary : array[0..255] of boolean;

  userev        : boolean;

  enabled_primary_ide   : boolean;
  enabled_secondary_ide : boolean;

  {$IfDef VirtualPascal}
  { could use dynamic allocation, but should be enough.. }
  tree          : array[1..2000] of treedata;
  {$Else}
  tree          : array[1.. 200] of treedata;
  {$EndIf}
  tree_used     : word;
  tree_used_empty : word; {used to count empty bus entries in tree[]}

  column_pos,             {only used for printstatus}
  column_pos_restart,
  numbers_of_screen_columns : word;

const
  { group consume-dissipate }
  power_management_readout_order:array[0..8] of byte=(0,4,1,5,2,6,3,7,8);

  bustype_names : array[bustype_types] of string[20]=
    ('PCI', (* bustype_PCI_known *)
     'PCI',
     'AGP',
     'CardBus',
     'PCI Express',
     'PCI-X',
     'None');

function Int2Str(i:longint) : String;
  var
    tmp                 :string;
  begin
    System.Str(i, tmp);
    Int2Str:=tmp;
  end;

function cvt4(const b:byte) : char;
  begin
    if b>9 then cvt4:=Chr(b+Ord('A')-10)
    else cvt4:=Chr(b+Ord('0'));
  end;

function wrhexb(const byt:byte): string;
  begin
    wrhexb:=cvt4(byt and $0f);
  end;

function wrhex(const byt:byte) : string;
  begin
    wrhex[0]:=#2;
    wrhex[1]:=cvt4((byt shr 4) and $0f);
    wrhex[2]:=cvt4( byt        and $0f);
  end;

function wrhexw(const wor:word): string;
  begin
    wrhexw[0]:=#4;
    wrhexw[1]:=cvt4((wor shr 12) and $0f);
    wrhexw[2]:=cvt4((wor shr  8) and $0f);
    wrhexw[3]:=cvt4((wor shr  4) and $0f);
    wrhexw[4]:=cvt4( wor         and $0f);
  end;

function wrhexl(const lon:longint): string;
  begin
    wrhexl:=wrhexw(lon shr 16)
           +wrhexw(lon and $ffff);
  end;

function x_byte_x_bytes(const b:word):string;
  var
    tmp                 :string;
  begin
    System.Str(b,tmp);
    tmp:=tmp+' byte';
    if b<>1 then tmp:=tmp+'s';
    x_byte_x_bytes:=tmp;
  end;

{ Make the PCI configuration status register printout pretty }
{ Input = the string to be output }

procedure printstatus (const s : string);
  begin

    if not first then
      if (column_pos+Length(', ')+Length(s))>=numbers_of_screen_columns then
        begin
          WriteLn(',');
          column_pos:=1;
          while column_pos<column_pos_restart do
            begin
              Write(' ');
              Inc(column_pos);
            end;
        end
      else
        begin
          Write(', ');
          Inc(column_pos,2);
        end;

    Write(s);
    Inc(column_pos,Length(s));
    first:=false;
  end;

procedure WriteColor(const s: string; a: byte);
  var
    oa: Byte;
  begin
    oa := TextAttr;
    TextAttr := (TextAttr and (not $0f)) or a;
    Write(s);
    TextAttr := oa;
  end;

procedure WritePCColor(const s: pChar; a: byte);
  var
    oa: Byte;
  begin
    oa := TextAttr;
    TextAttr := (TextAttr and (not $0f)) or a;
    Write(s);
    TextAttr := oa;
  end;

procedure WriteIntColor(const i: integer; a: byte);
  var
    oa: Byte;
  begin
    oa := TextAttr;
    TextAttr := (TextAttr and (not $0f)) or a;
    Write(i);
    TextAttr := oa;
  end;

procedure WriteLnColor(const s: string; a: byte);
  begin
    WriteColor(s,a);
    WriteLn;
  end;

{$IfDef VirtualPascal}
var
  pcidevs_txt_buffer    :PChar;
  pcidevs_txt_end       :PChar;
  pcidevs_txt_position  :PChar;
{$EndIf}

procedure Assign2(var f:text;const filename:string);
  {$IfDef VirtualPascal}
  var
    f2                  :file;
    f2s                 :longint;

  begin
    pcidevs_txt_buffer  :=nil;
    pcidevs_txt_end     :=nil;
    pcidevs_txt_position:=nil;

    FileMode:=FModeReadOnly;
    Assign(f2,filename);
    {$I-}
    Reset(f2,1);
    {$I+}
    if InOutRes<>0 then Exit;

    f2s:=FileSize(f2);
    GetMem(pcidevs_txt_buffer,f2s+3);
    pcidevs_txt_buffer[f2s  ]:=#$0d;
    pcidevs_txt_buffer[f2s+1]:=#$0a;
    pcidevs_txt_buffer[f2s+2]:=#$00;
    BlockRead(f2,pcidevs_txt_buffer[0],f2s);
    Close(f2);

    if not installermode then
      begin
        { '; This is version 474 of this file; 04-06-2004 (D-M-Y).' }
        pcidevs_txt_position:=StrPos(pcidevs_txt_buffer,#$0a'; This is version ');
        if Assigned(pcidevs_txt_position) then
          begin
            Inc(pcidevs_txt_position,11-8);
            Write('Loaded Vendor Data : "');
            while pcidevs_txt_position[0]>=' ' do
              begin
                Write(pcidevs_txt_position[0]);
                Inc(pcidevs_txt_position);
              end;
            WriteLn('"');
          end
        else
          WriteLnColor('Note: Please download a complete Vendor Data file (pcidevs.txt)!',LightRed);
      end;

    pcidevs_txt_end     :=@pcidevs_txt_buffer[f2s];
    pcidevs_txt_position:=pcidevs_txt_buffer;

  end;
  {$Else}
  begin
    FileMode:=FModeReadOnly;
    Assign(f,filename);
  end;
  {$EndIf}

{$IfNDef VirtualPascal}
var
  pcidevs_txt_read_buffer:array[0..32*1024-1] of byte;
{$EndIf}

procedure Reset2(var f:text);
  {$IfDef VirtualPascal}
  begin
    pcidevs_txt_position:=pcidevs_txt_buffer;
  end;
  {$Else}
  begin
    Reset(f);
    SetTextBuf(f,pcidevs_txt_read_buffer,SizeOf(pcidevs_txt_read_buffer));
  end;
  {$EndIf}


procedure ReadLn2(var f:text;var zk:string);
  {$IfDef VirtualPascal}
  var
    linestart:PChar;
    linelength:word;
  begin

    linestart:=pcidevs_txt_position;
    linelength:=0;
    while pcidevs_txt_position<>pcidevs_txt_end do
      case pcidevs_txt_position[0] of
        ^Z:
          Break;
        #13:
          Inc(pcidevs_txt_position);

        #10:
          begin
            Inc(pcidevs_txt_position);
            Break;
          end;
      else
        Inc(pcidevs_txt_position);
        Inc(linelength);
      end;

    if linelength=0 then
      zk:=''
    else
      begin
        SetLength(zk,linelength);
        Move(linestart^,zk[1],linelength);
      end;

  {$Else}
  begin
    ReadLn(f,zk);
  {$EndIf}

    if Length(zk)<3 then zk:=';   oops';
  end;



function Eof2(var f:text):boolean;
  {$IfDef VirtualPascal}
  begin
    Eof2:=(pcidevs_txt_position=pcidevs_txt_end) or (pcidevs_txt_position^=^Z);
  end;
  {$Else}
  begin
    Eof2:=Eof(f);
  end;
  {$EndIf}


procedure Close2(var f:text);
  {$IfDef VirtualPascal}
  begin
  end;
  {$Else}
  begin
    Close(f);
  end;
  {$EndIf}


procedure listmap(const va:word;const dispst:string);
  var
    comma  : byte;
    failed : boolean;
    l,
    j      : word;

  begin
    failed:=true;
    Write(dispst);
    comma:=0;
    for l:=0 to 15 do
      Inc(comma,(va shr l) and 1);

    for l:=0 to 15 do
      if Odd(va shr l) then
        begin
          Write(l);
          if comma>1 then Write(',')
          else Write(' ');
          Dec(comma);
          failed:=false;
        end;

    if failed then
      WriteLn('None')
    else
      WriteLn;
  end;

procedure listmap2(const va_is,va_should:word;const dispst:string);
  var
    comma  : byte;
    failed : boolean;
    l,
    j      : word;

  begin
    failed:=true;
    Write(dispst);
    comma:=0;
    for l:=0 to 15 do
      Inc(comma,(va_is shr l) and 1);

    for l:=0 to 15 do
      if Odd(va_is shr l) then
        begin
          if Odd(va_should shr l) then
            TextColor(LightGray)
          else
            TextColor(LightRed);
          Write(l);
          TextColor(LightGray);
          if comma>1 then Write(',')
          else Write(' ');
          Dec(comma);
          failed:=false;
        end;

    if failed then
      WriteLn('None')
    else
      WriteLn;
  end;


procedure lookupven(const silent:boolean);
  begin

    Reset2(f);
    failed:=true;

    repeat
      ReadLn2(f,vstr);
      if Length(vstr)<3 then vstr:=';   oops';
      if (vstr[1]='V') and (StrLComp(@vstr[3],@cmpstr[1],4)=0) then
        begin
          if not silent then WriteColor(Copy(vstr,8,Length(vstr)),Yellow);
          failed:=false;
          Break;
        end;
    until Eof2(f);

    if failed and (not silent) then
      WriteColor('Unknown',LightRed);
  end;

procedure lookupdev(const LengthLimit:word;const add_link_to_main_device_decoding:boolean);
  begin
    revchk:='';
    failed:=true;

    if dohtml and add_link_to_main_device_decoding then
      write_extra_html('<a href="#bus'+Int2Str(infotbl_bus)
                               +'_dev'+Int2Str(infotbl_deviceid)
                              +'_func'+Int2Str(infotbl_func)
                              +'">');

    while not Eof2(f) do
      begin

        ReadLn2(f,vstr);
        if Length(vstr)<3 then vstr:=';   oops';

        { 'D'^I'nnnn device name' }
        if (vstr[1]='D') and (StrLComp(@vstr[3],@cmpstr[1],4)=0) then
          begin
            if not Eof2(f) then ReadLn2(f,revchk);
            { 'R'^I'nn revision name' }
            while (revchk<>'') and (revchk[1]='R') do
              begin
                if wrhex(infotbl(8))=Copy(revchk,3,2) then
                  vstr:='xxxxxxx'+Copy(revchk,6,Length(revchk));
                if not Eof2(f) then
                  ReadLn2(f,revchk);
              end;
            { devince name/revision }
            Delete(vstr,1,1+1+4+1);

            { truncate to avoid line wrap in tables/trees }
            {$IfDef Debug}
            while Length(vstr)<LengthLimit do vstr:=vstr+'~';
            {$EndIf Debug}
            if Length(vstr)>LengthLimit then
              begin
                SetLength(vstr,LengthLimit-2);
                vstr:=vstr+'..';
              end;
            WriteColor(vstr,Yellow);
            failed:=false;
            Break; { success! }
          end;

      { next vendor begins, done. }
      if vstr[1]='V' then Break;
    end;

    if failed then
      WriteColor('Unknown',LightRed);

    if dohtml and add_link_to_main_device_decoding then
      write_extra_html('</a>'); { end href }

  end;

procedure ReadSubVendorDevice(var sub_vendor,sub_device:word);
  var
    cap_ptr             :word;

  begin

    sub_vendor:=0;
    sub_device:=0;

    case headertype of
      0:
        begin
          sub_vendor:=infotbl_W($2c);
          sub_device:=infotbl_W($2e);
        end;
      2:
        begin
          sub_vendor:=infotbl_W($40);
          sub_device:=infotbl_W($42);
        end;
    end;

    if (sub_vendor=0) and (sub_device=0) then
      begin

        cap_ptr:=first_cap_ptr;

        while cap_ptr_valid(cap_ptr) do
          if infotbl(cap_ptr)=$0d then { Subsystem ID & Subsystem Vendor ID }
            begin
              sub_vendor:=infotbl_W(cap_ptr + 4+0);
              sub_device:=infotbl_W(cap_ptr + 4+2);
              Break;
            end
          else
            cap_ptr:=infotbl(cap_ptr+1);

      end;

  end;


procedure showinstallerinfo;
  var
    sub_vendor,
    sub_device          :word;

  begin
    Write('V:',wrhexw(infotbl_W(0)),' ');

    Write('D:',wrhexw(infotbl_W(2)),' ');

    ReadSubVendorDevice(sub_vendor,sub_device);
    Write('S:',wrhexw(sub_vendor),wrhexw(sub_device),' ');

    Write('B:',infotbl_bus,' ');

    Write('E:');
    if infotbl_deviceid<10 then Write('0'); { 00..31 }
    Write(infotbl_deviceid,' ');

    Write('F:',infotbl_func,' ');

    Write('I:'); { Interrupt line }
    if infotbl($3c) in [1..254] then
      Write(wrhex(infotbl($3c)),' ')
    else
      Write('00 ');

    Write('N:'); { Interrupt pin }
    {if infotbl($3c) in [1..254] then
      begin
        if infotbl($3d)=0 then Write('- ')
        else Write(Chr(infotbl($3d)+Ord('@')),' ');
      end
    else Write('- ');}
    if infotbl($3d) in [1..4] then Write(Chr(infotbl($3d)+Ord('@')),' ')
    else                           Write('- ');

    Write('C:',wrhex(infotbl($b)),' ');

    Write('U:',wrhex(infotbl($a)),' ');

    Write('P:',wrhex(infotbl($9)),' ');

    write('R:',wrhex(infotbl($8))    );

    WriteLn;
  end;

const
  verify_routing_table_failed : boolean = false;

function verify_routing_table(const table:TIRQ_routing_table_entry_Array;slots:integer):boolean;
  var
    link_data           :array[byte] of word;
    link_data_initialized:array[byte] of integer;
    icbm1,icbm2         :word;
    i,j                 :integer;
  begin
    { previous call displayed errors? }
    if verify_routing_table_failed then
      begin
        verify_routing_table:=false;
        Exit;
      end;

    for i:=0 to slots-1-1 do
      with table[i] do
        if (PCI_bus_number<>0) or (PCI_device_number<>0) or (device_slot_number<>0) then
          for j:=i+1 to slots-1 do
            if   (table[j].PCI_bus_number         =PCI_bus_number         )
            and  (table[j].PCI_device_number shr 3=PCI_device_number shr 3) then
              begin
                verify_routing_table_failed:=true;
                TextColor(LightRed);
                WriteLn('  Error: table entries ',i,' and ',j,' refer to the same Bus=',
                  PCI_bus_number,'/Device=',PCI_device_number shr 3,'!');
                TextColor(LightGray);
              end;

    FillChar(link_data_initialized,SizeOf(link_data_initialized),$ff);
    for i:=0 to slots-1 do
      with table[i] do
        for j:=Low(INTABCD) to High(INTABCD) do
          with INTABCD[j] do
            begin
              if link_data_initialized[link_value]=-1 then
                begin
                  link_data[link_value]:=IRQ_connectivity_bit_map;
                  link_data_initialized[link_value]:=i*4+j;
                end
              else
              if link_value<>0 then
                begin
                  if link_data[link_value]<>IRQ_connectivity_bit_map then
                    begin
                      verify_routing_table_failed:=true;
                      TextColor(LightRed);
                      WriteLn('  Error: connectivity mismatches for link value ',wrhex(link_value),':');
                      TextColor(LightGray);

                      icbm1:=IRQ_connectivity_bit_map;

                      with table[link_data_initialized[link_value] div 4] do
                      with INTABCD[link_data_initialized[link_value] mod 4] do
                      begin
                      Write  ('    Bus=',PCI_bus_number,' Device=',PCI_device_number shr 3:2,
                              ' Pin=INT',Chr(Ord('@')+j+1),'# Map=');
                      icbm2:=IRQ_connectivity_bit_map;
                      TextColor(LightRed);
                      (*listmap(IRQ_connectivity_bit_map,'');*)
                      listmap2(icbm2,icbm1,'');
                      TextColor(LightGray);
                      end;
                      Write  ('    Bus=',PCI_bus_number,' Device=',PCI_device_number shr 3:2,
                              ' Pin=INT',Chr(Ord('@')+j+1),'# Map=');
                      TextColor(LightRed);
                      (*listmap(IRQ_connectivity_bit_map,'');*)
                      listmap2(icbm1,icbm2,'');
                      TextColor(LightGray);
                    end;
                end;
            end;
     verify_routing_table:=not verify_routing_table_failed;
  end;


procedure showroutinginfo;
  var
    tableok             :boolean;
    sum                 :byte;
    i,l,t               :word;

  const { list of known routers where we suspect that the BIOS has bad tables (0,0,0) }
    irq_router_table:array[1..5] of array[1..2] of smallword=(
      (* Bus 0 (PCI), Device Number 7, Device Function 3
         Vendor 1022h Advanced Micro Devices (AMD)
         Device 7413h AMD-766 Power Management Controller *)
      ($1022,$7413),

      (* Bus 0 (PCI), Device Number 7, Device Function 3
         Vendor 1022h Advanced Micro Devices (AMD)
         Device 7443h AMD-768 ACPI Controller *)
      ($1022,$7443),

      (*              AMD-8111 ACPI System Management Controller *)
      ($1022,$746B),

      (*  Bus 0 (PCI), Device Number 0, Device Function 0
          Vendor 10DEh Nvidia Corp
          Device 01E0h nForce2 AGP Controller

          Bus 0 (PCI), Device Number 1, Device Function 0
          Vendor 10DEh Nvidia Corp
          Device 0060h nForce2 ISA Bridge *)
      ($10DE,$0060),

      (*  Bus 0 (PCI), Device Number 0, Device Function 0
          Vendor 10DEh Nvidia Corp
          Device 00E1h nforce3 CPU to PCI Bridge
          PCI Class Bridge, type PCI to HOST

          Bus 0 (PCI), Device Number 1, Device Function 0
          Vendor 10DEh Nvidia Corp
          Device 00E0h nForce3 CPU to ISA Bridge
          PCI Class Bridge, type PCI to ISA *)

      ($10DE,$00E0)


       );

  begin

    if dohtml then
      writeln_extra_html('<a name="romrouting"></a>');

    (*WriteLn('ROM PCI IRQ routing table Windows 9x Compatibility Tests....');*)
    WriteLn('ROM PCI IRQ routing table Tests....');

{ Find table }

    i:=0;
    failed:=true;
    repeat
      if MemL_F000(i)=$52495024 { $PIR } then failed:=false;
      if failed then Inc(i,$10);
    until (i>$ffef) or (not failed);

{ check table }

    if failed then
      begin
        WriteLnColor('No ROM PCI IRQ routing table found!!!',LightRed);
        Exit;
      end;

    tableok:=true;


    WriteLn(' ROM IRQ routing table found at F000h:',wrhexw(i),'h');
    Write(' Table Version ',Mem_F000(i+5),'.',Mem_F000(i+4));
    if (Mem_F000(i+5)=1) and (Mem_F000(i+4)=0) then
      WriteLn(' - OK')
    else
      begin
        WriteLnColor('Invalid Version!',LightRed);
        tableok:=false;
      end;

    Write(' Table size ',MemW_F000(i+6),' bytes - ');
    if (MemW_F000(i+6)<33) or (MemW_F000(i+6) mod 16<>0) then
      begin
        WriteLnColor('Invalid Size!',LightRed);
        tableok:=false;
      end
    else
      WriteLn('OK');


    if tableok then
      begin
        Write(' Table Checksum ',wrhex(Mem_F000(i+31)),'h - ');
        {$R-}  {Range checking off as sum is DELIBERATELY meant to overfow }
        sum:=0;
        for l:=0 to MemW_F000(i+6)-1 do
          Inc(sum,Mem_F000(i+l));
        {$R+}
        if sum=0 then
          WriteLn('OK')
        else
          begin
            WriteLnColor('Failed!',LightRed);
            tableok:=false;
          end;
      end;

    if tableok then
      begin
        Write(' PCI Interrupt Router: ');
        set_lookup_device(Mem_F000(i+9) shr 3,Mem_F000(i+9) and 7,Mem_F000(i+8));

        if infotbl_W(0)=$ffff then
          WriteColor('No Device Detected!',LightRed)
        else
          begin
            cmpstr:=wrhexw(infotbl_W(0));
            lookupven(true);
            cmpstr:=wrhexw(infotbl_W(2));
            lookupdev(numbers_of_screen_columns-25,true);
          end;


        if (infotbl_bus=0) and (infotbl_deviceid=0) and (infotbl_func=0) then
          begin (* suspect wrong 0/0/0 entry *)

            for t:=Low(tree) to tree_used do
              with tree[t] do

                for l:=Low(irq_router_table) to High(irq_router_table) do
                  if  (vid=irq_router_table[l,1])
                  and (did=irq_router_table[l,2])
                  and ((b<>0) or (d<>0) or (f<>0)) then

                    begin
                      WriteLnColor(' (looks like an known error)',LightRed);
                      Write('               Router: ');
                      set_lookup_device(d,f,b);
                      cmpstr:=wrhexw(infotbl_W(0));
                      lookupven(true);
                      cmpstr:=wrhexw(infotbl_W(2));
                      lookupdev(numbers_of_screen_columns-25,true);
                      tableok:=false;
                    end;

          end;

        WriteLn;
      end;

    listmap(MemW_F000(i+10),' IRQ''s dedicated to PCI : ');

    if tableok then
      tableok:=verify_routing_table(TIRQ_routing_table_entry_Array(Ptr_F000(i+$20)^),(MemW_F000(i+6)-$20) div $10);

    if tableok then
      WriteLnColor(' The ROM PCI IRQ routing table appears to be OK.',LightGreen)
    else
      WriteLnColor(' The ROM PCI IRQ routing table appears to be faulty!!',LightRed);

  end;


procedure dohexdump;
  var
    i,j                 :word;

  begin
    WriteLn('Hex-Dump of IRQ Routing table : ');
    WriteLn;

    Write('  0000  ');

    for j:=0 to 15 do
      if j<6 then
        Write(wrhex(irqbuff[j]),' ')
      else
        Write('   ');

    Write('   ');

    for j:=0 to 6-1 do
      if irqbuff[j]<32 then
        Write('.')
      else
        Write(Chr(irqbuff[j]));

    WriteLn;


    for i:=Low(irqbuffR.IRQ_routing_table_entry_Array) to High(irqbuffR.IRQ_routing_table_entry_Array) do
      with irqbuffR.IRQ_routing_table_entry_Array[i] do
        begin
          Write('  ',wrhexw(i*16+6),'  ');

          for j:=0 to 15 do
            Write(wrhex(binary[j]),' ');

          Write('   ');

          for j:=0 to 15 do
            if binary[j]<32 then
              Write('.')
            else
              Write(Chr(binary[j]));

          WriteLn;
        end;

    WriteLn;
    WriteLn;
  end;


procedure docapdecode;
  const
    YesNo       :array[boolean] of string[3]=('No','Yes');
    Aux_Current :array[0..7] of word=(0,55,100,160,220,270,320,375);

  var
    lw,
    xx          :byte;
    reply       :string;
    pm_org,
    pm_work     :longint;
    cap_ptr     :byte;
    spl         :real;
    i,ii,j      :word;
    novpddata   :boolean;
    timeout     :longint;
    vpd         :array[byte] of byte;
    vp,
    upto        :word;
    key         :string[2];

  procedure output_vpd_hex;
    var
      i         :word;
      dup       :word;
    begin
      i:=1;
      while i<=vpd[vp+2] do
        begin
          if i+8<=vpd[vp+2] then
            begin
              dup:=1;
              while i+dup<=vpd[vp+2] do
                if vpd[vp+2+i+0]<>vpd[vp+2+i+dup] then
                  Break
                else
                  Inc(dup);
            end
          else
            dup:=0;

          if dup>=8 then
            begin
              Write(dup:2,'*',wrhex(vpd[vp+2+i]),'h ');
              Inc(i,dup);
            end
          else
            begin
              Write(wrhex(vpd[vp+2+i]),'h ');
              Inc(i);
            end;

        end;
    end;

  procedure output_vpd_chr;
    var
      i         :word;
    begin
      for i:=1 to vpd[vp+2] do
        Write(Chr  (vpd[vp+2+i]));
    end;


  begin
    writeln(' New Capabilities List Present:');

    cap_ptr:=first_cap_ptr;

    if cap_ptr=0 then
      begin
        WriteLn('  No ''New Capabilities'' Are Currently Enabled');
        Exit;
      end;


    repeat

      if not cap_ptr_valid(cap_ptr) then
        begin
          WriteColor('Cap_Ptr is invalid!', LightRed);
          Break;
        end;

      case infotbl(cap_ptr) of

        01 :
          begin
            Write('   Power Management Capability'); { "PMC", pm1_1.pdf page 25 }

            { registers:
               00 (B) ID(1)
               01 (B) next pointer
               02 (W) power management capabilities
               04 (W) power management control/status
               06 (B) bridge support extensions
               07 (B) data                              }

            case (infotbl(cap_ptr+2) and 7) of
              0:Write(', Version before 1.0?');
              1:Write(', Version 1.0');
              2:Write(', Version 1.1');
              3:Write(', Version 1.2');
            else
               Write(', unknown Version (later than 1.2: ',wrhexb(infotbl(cap_ptr+2) and 7),'h)');
            end;
            WriteLn;

{ list supported low-power states; D3 and D0 are always supported}
            if infotbl(cap_ptr+2+1) and 2=2 then WriteLn('     Supports low power State D1');
            if infotbl(cap_ptr+2+1) and 4=4 then WriteLn('     Supports low power State D2');
            if infotbl(cap_ptr+2+1) and 6=0 then WriteLn('     Does not support low power State D1 or D2');


{list PME# generation capabilities}
            if (infotbl(cap_ptr+2+1) shr 3) = 0 then
              WriteLn('     Does not support PME# signalling')
            else
             begin
               Write('     Supports PME# signalling from mode(s) ');
               column_pos:=1+Length('     Supports PME# signalling from mode(s) ');
               column_pos_restart:=1+Length('     ')+2;
               first:=true;
               if (infotbl(cap_ptr+3) shr 3) and  1= 1 then printstatus('D0');
               if (infotbl(cap_ptr+3) shr 3) and  2= 2 then printstatus('D1');
               if (infotbl(cap_ptr+3) shr 3) and  4= 4 then printstatus('D2');
               if (infotbl(cap_ptr+3) shr 3) and  8= 8 then printstatus('D3hot');
               if (infotbl(cap_ptr+3) shr 3) and 16=16 then printstatus('D3cold');
               WriteLn;

               write('     PME# signalling is currently ');
               if infotbl(cap_ptr+5) and 1=1 then Write('enabled')
               else                               Write('disabled');
               WriteLn;

               { Vaux is only interesting when PME# is supported,
                  known hardware errata (Intel PCI->PCI bridge) }
               {if (infotbl_w(cap_ptr+2) shr 6) and 7<>0 then}
               if (infotbl(cap_ptr+2+1) shr 3) <> 0 then
                 begin
                 (*Write('     current requirement for 3.3Vaux: '*)
                   Write('     3.3v AUX Current required : ');
                   Write(Aux_Current[(infotbl_w(cap_ptr+2) shr 6) and 7],' mA');
                   if ((infotbl_w(cap_ptr+2) shr 6) and 7)=0 then
                     Write(' (Self powered)');
                   WriteLn;
                 end;


             end;


            Write('     Current Power State : D');
            case infotbl(cap_ptr+4) and 3 of
              0 : Write('0 (Device operational, no power saving)');
              1 : Write('1 (Device idle, minimum power saving)');
              2 : Write('2 (Device CLK stopped, medium power saving)');
              3 : Write('3hot (Device off: no power to device, maximum power saving)');
            end;
            WriteLn;

            {$IfDef AskPowerChange}
            if not (ioredirected or readonly) then
              begin
                Write('New value (0=on 1=idle 2=medium 3=off) ');
                xx:=infotbl(cap_ptr+4) and (not 3);
                ReadLn(reply);
                if reply='0' then
                  write_byte(infotbl_deviceid,infotbl_func,infotbl_bus,cap_ptr+4,xx or 0)
                else
                if reply='1' then
                  write_byte(infotbl_deviceid,infotbl_func,infotbl_bus,cap_ptr+4,xx or 1)
                else
                if reply='2' then
                  write_byte(infotbl_deviceid,infotbl_func,infotbl_bus,cap_ptr+4,xx or 2)
                else
                if reply='3' then
                  write_byte(infotbl_deviceid,infotbl_func,infotbl_bus,cap_ptr+4,xx or 3);
                reset_infotbl_cache;
                Write(' New Current Power State : D');
                case infotbl(cap_ptr+4) and 3 of
                  0 : WriteLn('0 (Device operational, no power saving)');
                  1 : WriteLn('1 (Device idle, minimum power saving)');
                  2 : WriteLn('2 (Device CLK stopped, medium power saving)');
                  3 : writeln('3hot (Device off: no power to device, maximum power saving)');
                end;
              end;
            {$EndIf AskPowerChange}


            { can assume alignment mod 4=0 (spec) }
            pm_org:=infotbl_L(cap_ptr+4);
            if ((pm_org and 3)<3) and (not readonly) then
              begin { do not touch when device in is in D3 }
                pm_work:=pm_org and (3 shl 0+1 shl 8);{ PowerState/PM_En }

                {  255 shl 23  Data - read only
                   255 shl 16  PMCSR_BSE - read only bridge support
                     1 shl 15  PME_Status - sticky bit; WRITE-CLEAR
                     3 shl 13  Data_scale - read only
                    15 shl  9  Data_select - we write a new value
                     1 shl  8  PM_En - seem to be read/write?
                    63 shl  2  reserved
                     3 shl  0  PowerState                         }

                { D0/1/2/3-state power consumed in watts }
                for ii:=Low(power_management_readout_order) to High(power_management_readout_order) do
                  begin
                    i:=power_management_readout_order[ii];
                    write_dword(infotbl_deviceid,infotbl_func,infotbl_bus,cap_ptr+4,pm_work+(i shl 9));
                    reset_infotbl_cache;

                    { Select successful? }
                    if ((infotbl_L(cap_ptr+4) shr 9) and $f<>i) then
                      Continue;

                    case (infotbl_W(cap_ptr+4) shr 13) and 3 of
                      0:(*Write('???  ');*)
                        Continue; { Data/_Select/_Scale not implemented }
                      1:Write('     ',infotbl(cap_ptr+7)*100:5);
                      2:Write('     ',infotbl(cap_ptr+7)* 10:5);
                      3:Write('     ',infotbl(cap_ptr+7)*  1:5);
                    end;
                    Write(' mW ');
                    case i of
                      0,1,2,3:Write('D',i  ,' power consume');
                      4,5,6,7:Write('D',i-4,' power dissipate');
                      8:      Write('common logic power consume');
                    end;
                    WriteLn;
                  end;

                { data select 0 }
                { number of write access to this register is even
                   -- bug fix for mpeg cards in D3 mode .. }
                write_dword(infotbl_deviceid,infotbl_func,infotbl_bus,cap_ptr+4,pm_work+(0 shl 9));
                reset_infotbl_cache;

                if (pm_org and 3)<>(infotbl_L(cap_ptr+4) and 3) then
                  WriteColor('Error: During PM readout, the power state did change, please report!',LightRed);
              end; { power consume readout allowed case }
          end; { Power Management Capability }

        02 :
          begin
            Write('   AGP Capability, Version ');
            Write(infotbl(cap_ptr+2) shr 4,'.',infotbl(cap_ptr+2) and $0f,' ');

            if (infotbl(cap_ptr+2) shr 4)=1 then WriteLn('(AGP 1x and/or 2x support)');
            if (infotbl(cap_ptr+2) shr 4)=2 then WriteLn('(AGP 4x and below support)');
            if (infotbl(cap_ptr+2) shr 4)=3 then
              begin
                write('(AGP 8x and 4x, ');
                case infotbl(cap_ptr+2) and $0f of
                  0..4:Write('core');
                  5..9:Write('appendix');
                else
                       Write('??');
                end;
              WriteLn(' register support)');
            end;


            { Status register }

            Write('     AGP Speed(s) Supported : ');

            if infotbl(cap_ptr+4) and 8=8 then
              begin
                if infotbl(cap_ptr+4) and 1=1 then Write('4x ');
                if infotbl(cap_ptr+4) and 2=2 then Write('8x ');
                if infotbl(cap_ptr+4) and 7>3 then Write('Unknown Speed Reported (',wrhex(infotbl(cap_ptr+4) and 7),'h)!!');
              end
            else
              begin
                if infotbl(cap_ptr+4) and 1=1 then Write('1x ');
                if infotbl(cap_ptr+4) and 2=2 then Write('2x ');
                if infotbl(cap_ptr+4) and 4=4 then Write('4x ');
                if infotbl(cap_ptr+4) and 7=0 then
                  begin
                    WriteColor('None!!',LightRed);
                    WriteColor(' (Assume Only 1x Support)',LightCyan);
                  end;
              end;

            WriteLn;

            WriteLn('     FW Transfers Supported : ',
              YesNo[infotbl(cap_ptr+4) and $10=$10]);

            WriteLn('     >4GiB Address Space Supported : ',
              YesNo[infotbl(cap_ptr+4) and $20=$20]);

            WriteLn('     Sideband Addressing Supported : ',
              YesNo[infotbl(cap_ptr+5) and 2=2]);

{ if v3.? reported, see if v3.0 mode is on }
             if (infotbl(cap_ptr+2) shr 4)=3 then
               WriteLn('     AGP v3.0 Operation Mode Available : ',
                 YesNo[infotbl(cap_ptr+4) and 8=8]);

{ isosynch only in AGP v3.0 mode }
             if infotbl(cap_ptr+4) and 8=8 then
               WriteLn('     Isosynchronous Transactions Supported : ',
                 YesNo[infotbl(cap_ptr+6) and 2=2]);

            WriteLn('     Maximum Command Queue Length : ',x_byte_x_bytes(infotbl(cap_ptr+7)+1));


            { Command register }

            Write  ('     AGP Speed Selected : ');
          (*if infotbl(cap_ptr+8) and 8=8 then*)
            if infotbl(cap_ptr+4) and 8=8 then
              case infotbl(cap_ptr+8) and 7 of
                0:Write('None Selected');
                1:Write('4x ');
                2:Write('8x ');
              else
                  WriteColor('Unknown Speed Reported ('+wrhex(infotbl(cap_ptr+8) and 7)+'h)!!',LightRed);
              end
            else
              case infotbl(cap_ptr+8) and 7 of
                0:Write('None Selected');
                1:Write('1x ');
                2:Write('2x ');
                4:Write('4x ');
              else
                  WriteColor('Unknown Speed Reported ('+wrhex(infotbl(cap_ptr+8) and 7)+'h)!!',LightRed);
              end;
            WriteLn;

            WriteLn('     FW Transfers Enabled : ',
              YesNo[infotbl(cap_ptr+8) and $10=$10]);

            WriteLn('     >4GiB Address Space Enabled : ',
              YesNo[infotbl(cap_ptr+8) and $20=$20]);

            Write  ('     AGP Enabled : ');
            if infotbl(cap_ptr+9) and 1=1 then
              WriteLnColor('Yes',LightGreen)
            else
              WriteLnColor('No',LightRed);

            WriteLn('     Sideband Addressing Enabled : ',
              YesNo[infotbl(cap_ptr+9) and 2=2]);

            if infotbl(cap_ptr+4) and 8=8 then
              begin
                Write('     AGP v3.0 Operation Mode : ');
                if infotbl(cap_ptr+9) and 1=1 then WriteLn('Enabled') else WriteLn('Disabled');
              end;

            WriteLn('     Current Command Queue Length : ',x_byte_x_bytes(infotbl(cap_ptr+11)+1));
          end;


        03 :
          begin
            WriteLn('   Vital Product Data Capability');

            if not readonly then
              begin
                failed:=false;
                novpddata:=false;
                i:=0;
                repeat
                  write_dword(infotbl_deviceid,infotbl_func,infotbl_bus,cap_ptr,i shl (2+16)+infotbl_W(cap_ptr));
                  timeout:=0;
                  repeat
                    reset_infotbl_cache;
                    inc(timeout);
                  until (infotbl(cap_ptr+3) and $80=$80) or (timeout=1000000);

                  {$IfDef Debug}
                  if timeout=1000000 then
                    WriteColor('T',LightRed)
                  else
                    Write('*');
                  {$EndIf Debug}

                  if timeout=1000000 then failed:=true;

                  vpd[i shl 2 + 0]:=infotbl(cap_ptr+4+0);
                  vpd[i shl 2 + 1]:=infotbl(cap_ptr+4+1);
                  vpd[i shl 2 + 2]:=infotbl(cap_ptr+4+2);
                  vpd[i shl 2 + 3]:=infotbl(cap_ptr+4+3);

                  {$IfDef Debug}
                  Write(i:2,':',wrhex(vpd[i shl 2 + 0]),' ',
                                wrhex(vpd[i shl 2 + 1]),' ',
                                wrhex(vpd[i shl 2 + 2]),' ',
                                wrhex(vpd[i shl 2 + 3]),' ');
                  {$EndIf Debug}


                  if i=0 then
                    if (vpd[0]=vpd[1]) and (vpd[1]=vpd[2]) and (vpd[2]=vpd[3]) then
                      begin
                        failed:=true;
                        novpddata:=true;
                        WriteLn('     VPD Data not present');
                      end;

                  Inc(i);

                until (i=$40) or failed;

                {$IfDef Debug}
                WriteLn;
                {$EndIf Debug}


                if not novpddata then
                  begin

                    if dumpregs then
                      begin
                        WriteLn;
                        WriteLn(' Hex-Dump of vpd space follows:');
                        for i:=0 to 15 do
                          begin
                            Write('    ',wrhexw(i*16),'  ');
                            for j:=i*16 to i*16+15 do
                              Write(wrhex(vpd[j]),' ');
                            Write('   ');
                            for j:=i*16 to i*16+15 do
                              if vpd[j]<32 then
                                Write('.')
                              else
                                Write(Chr(vpd[j]));
                            WriteLn;
                          end;
                        WriteLn;
                      end;

                    {$IfDef Debug}
                    WriteLn('!!Decode Begins');
                    {$EndIf Debug}

                    vp:=0;

                    repeat

                      if vpd[vp] and $80=0 then
                        begin
                          { small }
                          if (vpd[vp] and $7f) shr 3=$0f then
                            begin
                              { end }
                              WriteLn('     End Tag');
                              vp:=0;
                            end
                          else
                            vp:=vp+(vpd[vp] and 7)+1;
                        end
                      else
                        { big }
                        case vpd[vp] and $7f of
                          $02:
                            begin
                              { string }
                              Write('     Identifier          ');
                              for i:=vp+3 to vp+2+(vpd[vp+2] shl 8 + vpd[vp+1]) do
                                Write(Chr(vpd[i]));
                              WriteLn;
                              vp:=vp+(vpd[vp+2] shl 8 + vpd[vp+1])+3;
                            end;

                          $10:
                            begin
                              { VPD-R }
                              {$IfDef Debug}
                              WriteLn('VPD-R Data');
                              {$EndIf Debug}
                              upto:=vp+(vpd[vp+2] shl 8 + vpd[vp+1])+3;
                              vp:=vp+3;
                              repeat
                                {$IfDef Debug}
                                WriteLn('Keyword : ',Chr(vpd[vp]),Chr(vpd[vp+1]),' Length : ',wrhex(vpd[vp+2]),'h : ');
                                {$EndIf Debug}
                                key:=Chr(vpd[vp])+Chr(vpd[vp+1]);
                                Write('     ');
                                if key   ='PN' then Write('Part Number         ');
                                if key   ='EC' then Write('EC Level (Version)  ');
                                if key   ='MN' then Write('Manufacturer ID     ');
                                if key   ='SN' then Write('Serial Number       ');
                                if key[1]='V'  then Write('Vendor Specific     ');
                                if key[1]='Y'  then Write('System Specific     ');
                                if key   ='RV' then Write('Checksum/End        ');
                                if key   ='RW' then Write('Remaining RW Area   ');
                                if key   ='CP' then Write('Extended Capability ');
                                if key   ='PG' then Write('PCI Geography       ');
                                if key   ='LC' then Write('Location            ');
                                if key   ='FG' then Write('Fabric Geography    ');
                                if (key='FG') or (key='LC') or (key='CP') or (key='RV') or (key='RW') then
                                  output_vpd_hex
                                else
                                  output_vpd_chr;
                                Writeln;
                                vp:=vp+3+vpd[vp+2];
                              until vp=upto;
                            end;

                          $11:
                            begin
                              { VPD-W }
                              {$IfDef Debug}
                              WriteLn('VPD-W Data');
                              {$EndIf Debug}
                              upto:=vp+(vpd[vp+2] shl 8 + vpd[vp+1])+3;
                              vp:=vp+3;
                              repeat
                                {$IfDef Debug}
                                WriteLn('Keyword : ',Chr(vpd[vp]),Chr(vpd[vp+1]),' Length : ',wrhex(vpd[vp+2]),'h : ');
                                {$EndIf Debug}
                                key:=Chr(vpd[vp])+Chr(vpd[vp+1]);
                                Write('     ');
                                if key   ='PN' then Write('Part Number         ');
                                if key   ='EC' then Write('EC Level (Version)  ');
                                if key   ='MN' then Write('Manufacturer ID     ');
                                if key   ='SN' then Write('Serial Number       ');
                                if key[1]='V'  then Write('Vendor Specific     ');
                                if key[1]='Y'  then Write('System Specific     ');
                                if key   ='RV' then Write('Checksum/End        ');
                                if key   ='RW' then Write('Remaining RW Area   ');
                                if key   ='CP' then Write('Extended Capability ');
                                if key   ='PG' then Write('PCI Geography       ');
                                if key   ='LC' then Write('Location            ');
                                if key   ='FG' then Write('Fabric Geography    ');
                                if (key='FG') or (key='LC') or (key='CP') or (key='RV') or (key='RW') then
                                  output_vpd_hex
                                else
                                  output_vpd_chr;
                                WriteLn;
                                vp:=vp+3+vpd[vp+2];
                              until vp=upto;
                            end;

                        end; (* case vpd[vp] and $7f of *)

                    until vp=0;

                  end; (* not novpddata *)

              end; (* not readonly *)

          end;


        04 :
          begin
            WriteLn('   Slot Identification Capability');

            Write  ('     This is ');
            if infotbl(cap_ptr)+2 and $20=0 then Write('not ');
            WriteLn('a parent bridge');

            Write  ('     Number of slots on secondary side of this bridge : ');
            WriteLn(infotbl(cap_ptr+2) and $1f);

            WriteLn('Chassis Number : ',infotbl(cap_ptr+3));
          end;


        05 :
          begin
            WriteLn('   Message Signalled Interrupt Capability');
            Write  ('     MSI is ');
            if infotbl(cap_ptr+2) and 1=1 then WriteLn('enabled') else WriteLn('disabled');

            Write  ('     MSI function can generate ');
            if infotbl(cap_ptr+2) and 128=128 then Write('64') else Write('32');
            WriteLn('-bit addresses');
          end;


        06 :
          begin
            WriteLn('   CompactPCI Hot-Swap Capability');
          end;


        07 :
          begin
            WriteLn('   PCI-X Capability');

{ type 1 }
            if headertype=1 then
              begin
                Write  ('     Secondary AD Interface Size is ');
                if infotbl(cap_ptr+2) and 1=1 then Write('64') else Write('32');
                WriteLn('-bits wide');

                Write  ('     Secondary Bus Maximum Speed in PCI-X Mode 1 is ');
                if infotbl(cap_ptr+2) and 2=2 then Write('133') else Write('66');
                Writeln('MHz');

                WriteLn('     Secondary Bus is PCI-X 266 Capable : ',
                  YesNo[infotbl(cap_ptr+3) and $40=$40]);
                WriteLn('     Secondary Bus is PCI-X 533 Capable : ',
                  YesNo[infotbl(cap_ptr+3) and $80=$80]);

                Write  ('     Secondary Bus Current Speed : ');
                case (Word(infotbl(cap_ptr+3) and 3) shl 2 + (infotbl(cap_ptr+2) and $c0) shr 6) of
                  0 : Write('PCI (33MHz)');
                  1 : Write('PCI-X Mode 1 (66MHz)');
                  2 : Write('PCI-X Mode 1 (100MHz)');
                  3 : Write('PCI-X Mode 1 (133MHz)');
               4..7 : Write('PCI-X Mode 1 (Unknown Speed!)');
                  8 : Write('PCI-X 266 (Unknown Speed!)');
                  9 : Write('PCI-X 266 (66MHz)');
                 $a : Write('PCI-X 266 (100MHz)');
                 $b : Write('PCI-X 266 (133MHz)');
                 $c : Write('PCI-X 533 (Unknown Speed!)');
                 $d : Write('PCI-X 533 (66MHz)');
                 $e : Write('PCI-X 533 (100MHz)');
                 $f : Write('PCI-X 533 (133MHz)');
                end;
                WriteLn;

              end;

{ type 0 and 1 }
            Write  ('     Primary AD Interface size is ');
            if infotbl(cap_ptr+6) and 1=1 then Write('64') else Write('32');
            WriteLn('-bits wide');

            Write  ('     Primary AD Bus Maximum Speed in PCI-X mode 1 is ');
            if infotbl(cap_ptr+6) and 2=2 then Write('133') else Write('66');
            WriteLn('MHz');

            WriteLn('     Primary Bus is PCI-X 266 Capable : ',
              YesNo[infotbl(cap_ptr+7) and $40=$40]);
            WriteLn('     Primary Bus is PCI-X 533 Capable : ',
              YesNo[infotbl(cap_ptr+7) and $80=$80]);

          end;

        08 :
          begin
            WriteLn('   HyperTransport Capability');
            Write('     SubType : ');
            if infotbl(cap_ptr+3) and $e0=$00 then
              Write('Slave/Primary Interface')
            else
            if infotbl(cap_ptr+3) and $e0=$20 then
              Write('Host/Secondary Interface')
            else
              case infotbl(cap_ptr+3) and $F8 of
                $40:Write('Switch');
                $80:Write('Interrupt Discovery & Configuration');
                $88:Write('Revision ID');
                $90:Write('UnitID Clumping');
                $98:Write('Extended Configuration Space Access');
                $A0:Write('Address Mapping');
                $A8:Write('MSI Mapping');
                $B0:Write('DirectRoute');
                $B8:Write('VCSet');
                $C0:Write('Retry mode');
                $C8:Write('x86 Encoding(Reserved)');
              else
                    Write('?? Unknown');
              end;
            WriteLn;

{ Slave/Pri : type 000xx}
            if infotbl(cap_ptr+3) and $e0=0 then
              begin
                WriteLn('     Base UnitID  : ',infotbl(cap_ptr+2) and $1f);
                WriteLn('     UnitID Count : ',((infotbl(cap_ptr+3) and 3) shl 3) + (infotbl(cap_ptr+2) shr 5));
              end;

          end;

        09 :
          begin
            WriteLn('   Vendor-Dependant Capability');
          end;

        $0a :
          begin
            WriteLn('   USB 2.0 EHCI Debug Port Capability');
          end;

        $0b :
          begin
            WriteLn('   CompactPCI Resource Control Capability');
          end;

        $0c :
          begin
            WriteLn('   PCI Hot-Plug Capability');
          end;

        $0d :
          begin
            WriteLn('   Subsystem ID & Subsystem Vendor ID Capability');
            WriteLn('     SSVID : ',wrhexw(infotbl_W(cap_ptr + 4)),'h');
            WriteLn('     SSID  : ',wrhexw(infotbl_W(cap_ptr + 6)),'h');
          end;

        $0e :
          begin
            WriteLn('   AGP 8x Capability');
          end;

        $0f :
          begin
            WriteLn('   Secure Device Capability');
          end;

        $10 :
          begin
            WriteLn('   PCI Express Capability, Version ',infotbl(cap_ptr+2) and $f);

   { **** 3GIO Capabilities field 2-3 }
            WriteLn('     Device/Port Type : ');
            Write  ('       ');
            case infotbl(cap_ptr+2) shr 4 of
              0 : Write('PCI Express Endpoint Device');
              1 : Write('Legacy PCI Express Endpoint Device');
              4 : Write('Root port of PCI Express Root Complex');
              5 : Write('Upstream port of PCI Express Switch');
              6 : Write('Downstream port of PCI Express Switch');
              7 : Write('PCI Express to PCI/PCI-X Bridge');
              8 : Write('PCI/PCI-X to PCI Express Bridge');
              9 : Write('Root Complex Integrated Endpoint Device');
            else  Write('Unknown (',wrhex(infotbl(cap_ptr+2) shr 4),'h)!!');
            end;
            WriteLn;

   { ports only: 4,5 (6?) }
            if (infotbl(cap_ptr+2) shr 4) in [4,6] then
              begin
                WriteLn('     Port Type :');
                Write  ('       Port is an ');
                if infotbl(cap_ptr+3) and 1=1 then
                  Write('Expansion Slot')
                else
                  Write('Integrated Device');
                WriteLn;
              end;

   { **** Device Capabilities field 4-7}
   { Types: 0,1,5,7 }
            if (infotbl(cap_ptr+2) shr 4) in [0,1,5,7] then
              begin
                WriteLn('     Device Capabilities :');
                if infotbl(cap_ptr+5) and 16=16 then WriteLn('       Attention Button Present on Device');
                if infotbl(cap_ptr+5) and 32=32 then WriteLn('       Attention Indicator Present on Device');
                if infotbl(cap_ptr+5) and 64=64 then WriteLn('       Power Indicator Present on Device');
              end;

   { upstream ports only: 5 }
            if infotbl(cap_ptr+2) shr 4=5 then
              begin
                WriteLn('     Upstream Port Power Data :');
                Write  ('       Slot Power Limit Value : ');
                spl:=(infotbl(cap_ptr+6) shr 2) + ((infotbl(cap_ptr+7) and 3) shl 6);
                if ((infotbl(cap_ptr+7) and $c) shr 2)=0 then spl:=spl;
                if ((infotbl(cap_ptr+7) and $c) shr 2)=1 then spl:=spl*0.1;
                if ((infotbl(cap_ptr+7) and $c) shr 2)=2 then spl:=spl*0.01;
                if ((infotbl(cap_ptr+7) and $c) shr 2)=3 then spl:=spl*0.001;
                WriteLn(spl:7:3,' Watts');
              end;

   { **** Device Control Field 8-9}
            if infotbl(cap_ptr+8) and  1= 1 then WriteLn('       Correctable Error Reporting Enabled');
            if infotbl(cap_ptr+8) and  2= 2 then WriteLn('       Non-Fatal Error Reporting Enabled');
            if infotbl(cap_ptr+8) and  4= 4 then WriteLn('       Fatal Error Reporting Enabled');
            if infotbl(cap_ptr+8) and  8= 8 then WriteLn('       Unsupported Request Reporting Enabled');
            Write('       Unsupported Request Severity is ');
            if infotbl(cap_ptr+8) and 16=16 then WriteLn('Fatal') else WriteLn('Non-Fatal');


   { **** Device Status field 0ah-0bh}
            WriteLn('     Device Status :');
            if infotbl(cap_ptr+$a) and  1= 1 then WriteLn('       Correctable Error Detected');
            if infotbl(cap_ptr+$a) and  2= 2 then WriteLn('       Non-Fatal Error Detected');
            if infotbl(cap_ptr+$a) and  4= 4 then WriteLn('       Fatal Error Detected');
            if infotbl(cap_ptr+$a) and  8= 8 then WriteLn('       Unsupported Request Detected');
            if infotbl(cap_ptr+$a) and 16=16 then WriteLn('       AUX Power Detected');
            if infotbl(cap_ptr+$a) and 32=32 then WriteLn('       Device Reports Transactions Pending');


   { **** Link Capabilities field 0ch-0fh}
            WriteLn('     Link Capabilities : ');
            Write  ('       Maximum Link speed : ');
            case infotbl(cap_ptr+$c) and $f of
              1 : Write('2.5Gb/s');
            else  Write('Unknown (',wrhex(infotbl(cap_ptr+$c) and $f),'h)!!');
            end;
            WriteLn;

            Write  ('       Maximum Link Width : x');
            lw:=((infotbl(cap_ptr+$d) and 3) shl 4) + ((infotbl(cap_ptr+$c) and $f0) shr 4);
            if lw=0 then Write('Reserved') else Write(lw);
            WriteLn;

            WriteLn('       Link Port Number   : ',infotbl(cap_ptr+$f));

   { **** Link Control 10h-11h }
            WriteLn('     Link Control :');
            if infotbl(cap_ptr+$10) and  4= 4 then WriteLn('       Link is in Loopback mode');
            if infotbl(cap_ptr+$10) and 16=16 then WriteLn('       Link is Disabled');
            if infotbl(cap_ptr+$10) and 64=64 then WriteLn('       Common Clock Configuration In Use')
                                              else WriteLn('       Asynchronous Clocking in Use');


   { **** Link Status 12h-13h }
            WriteLn('     Link Status :');
            Write  ('       Current Link speed : ');
            case infotbl(cap_ptr+$12) and $f of
              1 : Write('2.5Gb/s');
            else  Write('Unknown (',wrhex(infotbl(cap_ptr+$12) and $f),')!!');
            end;
            WriteLn;

            Write  ('       Current Link Width : x');
            lw:=((infotbl(cap_ptr+$13) and 3) shl 4) + ((infotbl(cap_ptr+$12) and $f0) shr 4);
            if infotbl(cap_ptr+$13) and 8=8 then WriteLn('??') else WriteLn(lw);
            if infotbl(cap_ptr+$13) and 4=4 then WriteLn('     Link Training Error Reported!!');
            if infotbl(cap_ptr+$13) and 8=8 then WriteLn('     Link Training Currently In Progress!!');

   { **** Slot Capabilities 14h-17h, root & downstream ports that are slots only }
            if ((infotbl(cap_ptr+2) shr 4) in [4,6]) and (infotbl(cap_ptr+3) and 1=1) then
              begin
                WriteLn('     Slot Capabilities :');
                if infotbl(cap_ptr+$14) and  1= 1 then WriteLn('       Attention Button Present');
                if infotbl(cap_ptr+$14) and  2= 2 then WriteLn('       Power Controller Present');
                if infotbl(cap_ptr+$14) and  4= 4 then WriteLn('       MRL Sensor Present');
                if infotbl(cap_ptr+$14) and  8= 8 then WriteLn('       Attention Indicator Present');
                if infotbl(cap_ptr+$14) and 16=16 then WriteLn('       Power Indicator Present');
                if infotbl(cap_ptr+$14) and 32=32 then WriteLn('       Hot Plug Surprise is Possible');
                if infotbl(cap_ptr+$14) and 64=64 then WriteLn('       Hot Plug Capable');

                Write  ('       Slot Power Limit Value : ');
                spl:=(infotbl_W(cap_ptr+14) shr 7) and $ff;
                case (infotbl_W(cap_ptr+15) shr 7) and 3 of
                  0:spl:=spl;
                  1:spl:=spl*0.1;
                  2:spl:=spl*0.01;
                  3:spl:=spl*0.001;
                end;
                writeln(spl:7:3,' Watts');

                WriteLn('       Physical slot Number ',(word(word(infotbl(cap_ptr+$17)) shl 2)+(infotbl(cap_ptr+$16) shr 6)));
              end;

          end;


        $11 :
          begin
            WriteLn('   MSI-X Capability');
          end;

      else
            WriteLn('   Unknown Capability (Code ',wrhex(infotbl(cap_ptr)),'h)!!');
      end;

      cap_ptr:=infotbl(cap_ptr+1);

    until cap_ptr=0;
  end;


procedure print_decodestatus(const status:word);
  begin

    Write(wrhexw(status),'h');Inc(column_pos,4+1);

    if status=0 then Exit;

    Write(' (');Inc(column_pos,1+1);
    first:=true;

    { Bit 3?..0 reserved }
    if Odd(status shr  3) then printstatus('Signalled Interrupt');
    if Odd(status shr  4) then printstatus('Has Capabilities List');
    if Odd(status shr  5) then printstatus('Supports 66MHz');
    if Odd(status shr  6) then printstatus('Supports UDF');
    if Odd(status shr  7) then printstatus('Supports Back-To-Back Trans.');

    if Odd(status shr  8) then printstatus('Data parity Error Detected');
    { timing 2/4}
    if Odd(status shr 11) then printstatus('Signalled Target Abort');
    if Odd(status shr 12) then printstatus('Received Target Abort');
    if Odd(status shr 13) then printstatus('Received Master Abort');
    if Odd(status shr 14) then printstatus('Signalled System Error'); { 'Received System Error' }
    if Odd(status shr 15) then printstatus('Detected Parity Error');

    case (status shr 9) and 3 of
      0 : printstatus('Fast Timing');
      1 : printstatus('Medium Timing');
      2 : printstatus('Slow Timing');
      3 : printstatus('Unknown Timing');
    end;
    Write(')');

  end;

procedure print_decodebridgecontrol(const control:word;const devtype:byte);
  begin

    Write(wrhexw(control),'h');Inc(column_pos,4+1);
    if control=0 then Exit;

    Write(' (');Inc(column_pos,1+1);
    first:=true;

    if Odd(control shr  0) then printstatus('parity detection');
    { reserved, cardbus }
    if Odd(control shr  1) then printstatus('CSERR forwarded');
    if Odd(control shr  2) then printstatus('ISA mapping');
    if Odd(control shr  3) then printstatus('VGA mapping');
    { reserved }
    if Odd(control shr  5) then printstatus('master abort mode');
    if Odd(control shr  6) then printstatus('bus is in RESET');
    case devtype of
      2:
        begin
          if Odd(control shr  7) then printstatus('ints routed by ExCA')
          else                        printstatus('ints routed by PCI');
        end;
    else
      if Odd(control shr  7) then printstatus('back-to-back transactions');
    end;
    (* cardbus:
    if Odd(control shr  8) then printstatus('memory 0 prefetchable');
    if Odd(control shr  9) then printstatus('memory 1 prefetchable'); *)
    if Odd(control shr 10) then printstatus('write post enable');
    { 15..11 reserved }
    Write(')');

  end;

function has_agp_capability:boolean;
  var
    cap_ptr             :byte;

  begin
    has_agp_capability:=false;

    cap_ptr:=first_cap_ptr;

    while cap_ptr_valid(cap_ptr) do
      if infotbl(cap_ptr)=02 then { AGP }
        begin
          { any speed supported ? }
          has_agp_capability:=(infotbl(cap_ptr+4) and 7<>0);
          Exit;
        end
      else
        cap_ptr:=infotbl(cap_ptr+1);

  end;

procedure look_if_ide_channels_enabled;
  begin
    enabled_primary_ide   :=true;
    enabled_secondary_ide :=true;


    case infotbl_W($00) of
      $1106,
      $1022:
        case infotbl_W($02) of
          {  VIA/AMD(?):
          Format of AMD-645 Peripheral Bus Controller, function 1 (IDE Control) data:
          Offset  Size    Description     (Table 01034)
           40h    BYTE    chip enable (see #01035)

          Bitfields for AMD-645 IDE Chip Enable register:
          Bit(s)  Description     (Table 01035)
           7-2    reserved (00001)
           1      enable primary IDE channel
           0      enable secondary IDE channel }

          {if (infotbl($40) shr 2)=1 then}
          $0571,
          $1571,
          $3149,
          $3164,
          $4149:
            begin
              enabled_primary_ide  :=Odd(infotbl($40) shr 1);
              enabled_secondary_ide:=Odd(infotbl($40) shr 0);
            end;
        end; { VIA/AMD }

      $8086: { Intel }
        case infotbl_W($02) of
          $1230,
          $2411,
          $2421,
          $2441,
          $244a,
          $244b,
          $245a,
          $245b,
          $248a,
          $248b,
          $24ca,
          $24cb,
          $24d1,
          $24db,
          $24df,
          $25a2,
          $25a3,
          $25b0,
          $2651,
          $2652,
          $266f,
          $7010,
          $7111,
          $7199,
          $7601:
            begin
              { Format of PCI Configuration Data for Intel 82371AB (PIIX4), IDE Controller:
                 Offset  Size    Description     (Table 01103)
                  40h    WORD    primary channel timing register
                  42h    WORD    secondary channel timing register
                -> IDE Decode Enable }
              enabled_primary_ide  :=Odd(infotbl($41) shr 7);
              enabled_secondary_ide:=Odd(infotbl($43) shr 7);
            end;
        end; { Intel }

      $1095: { CMD/Silicon Image }
        case infotbl_W($02) of
          $0640,
          $0643,
          $0646,
          $0648,
          $0649:
            begin { channel enable bits }
              enabled_primary_ide  :=Odd(infotbl($51) shr 2);
              enabled_secondary_ide:=Odd(infotbl($51) shr 3);
            end;
        end; { CMD }

      $1039: { SiS }
        case infotbl_W($02) of
          $0180,
          $0181,
          $5513,
          $5517,{?}
          $5518:
            begin { unknown }
              enabled_primary_ide  :=Odd(infotbl($4a) shr 1);
              enabled_secondary_ide:=Odd(infotbl($4a) shr 2);
            end;

        end; { SiS }

(*   $1025: { Acer }
        begin
        end;*)
(*   $105a: { Promise }
        begin
        end;*)
    end; { case vendor }

  end;

procedure determine_pci_or_pci_express(const bus:byte);
  var
    deviceid            :byte;
    cap_ptr             :word;
  begin
    { cosmetic problem: for skt775.log the first device on bus 0
      - Intel 915G Grantsdale CPU to I/O Controller - has no
      PCI Express Capability. device 1 has, so only device 0
      is declared on ' Bus 0 (PCI)', all later are ' Bus 0 (PCI Express)'}

    for deviceid:=0 to $1f do
      begin

        if not (bustype[bus] in [bustype_PCI_known,
                                 bustype_PCI,
                                 bustype_none      ]) then Exit;

        { only look at function 0 (speedup) }
        set_lookup_device(deviceid,0,bus);

        if infotbl_W(0)=$ffff then
          Continue;


        cap_ptr:=first_cap_ptr;
        while cap_ptr_valid(cap_ptr) do
          begin
            if infotbl(cap_ptr)=$10 then
              begin
                bustype[bus]:=bustype_PCI_E;
                Break;
              end;
            cap_ptr:=infotbl(cap_ptr+1);
          end;
      end;
  end;


procedure showallinfo;
  var
    cap_ptr             : byte;
    x                   : byte;
    nn,
    pp,ppused,
    lb                  : byte;

    search_class,
    search_subclass,
    search_progif       : byte;

    sub_vendor,
    sub_device          : word;
    mb,                         { membegin }
    ml                  : word; { memlimit }
    mbh,
    mlh                 : longint;
    bogusid,
    genssid,
    found,
    gotit               : boolean;

    romsize,
    romresult,
    addr,
    addrhi,
    size,
    org_rom_addr        : longint;

    oemidnum,
    oemidstr            : string;
    i,j                 : word;


  procedure show_address_range(const window_num:word;baseaddr,limit:longint;
                               const io_type:boolean;const prefetch:boolean);
    const
      prefetch_message:array[boolean] of pChar=('',', prefetchable');
    begin

     {if io_type then
         for an TI 1225 chip I/O range, bit 0/1 are hardcoded to zero
          but it is possible to write 32 bit addresses (no idea how to test
          if it works). So this code is questionable.
        case baseaddr and 3 of
          1:
            wide:=' (32-bit)';
        else
            baseaddr:=baseaddr and $0000ffff;
            limit   :=limit    and $0000ffff;
            wide:=' (16-bit)';
        end;}


      if io_type then
        Write('   I/O Port')
      else
        Write('   Memory  ');
      Write(' Range ',window_num,{too long:' Passed to Secondary Bus : ',}' : ');

      if (baseaddr>limit) or (limit=0) then
        begin
          WriteLn('None');
          Exit;
        end;

      if io_type then
        limit:=limit or 3 { DWords allowed }
      else
        limit:=limit or $fff; { 4 KiB granularity - for CardBus }

      if io_type and ((limit and $ffff0000)=0) then { not print leading zeroes for 16 bit I/O ranges }
        Write(wrhexw(baseaddr and $fffffffc),'h'+chars_to_,wrhexw(limit),'h')
      else
        Write(wrhexl(baseaddr and $fffffffc),'h'+chars_to_,wrhexl(limit),'h');

      if not io_type then
        Write(prefetch_message[prefetch]);
      WriteLn;
    end;

  begin


    if dohtml then
      write_extra_html('<a name="bus'+Int2Str(infotbl_bus)
                              +'_dev'+Int2Str(infotbl_deviceid)
                             +'_func'+Int2Str(infotbl_func)+'"></a>');

    if businfo then
      begin
        if dohtml and showtree then
          write_extra_html('<a href="#bus'+Int2Str(infotbl_bus)+'">');

        Write(' Bus ');
        WriteIntColor(infotbl_bus,LightCyan);

        if dohtml and showtree then
          write_extra_html('</a>');

        Write(' (',bustype_names[bustype[infotbl_bus]],'), Device Number ');
        WriteIntColor(infotbl_deviceid,LightCyan);
        Write(', Device Function ');
        WriteIntColor(infotbl_func,LightCyan);
        WriteLn;
      end; { businfo }


    if installermode then
      showinstallerinfo
    else
      begin

        Write(' Vendor ',wrhexw(infotbl_W(0)),'h ');
        cmpstr:=wrhexw(infotbl_W(0));
        lookupven(false);
        WriteLn;


        Write(' Device ',wrhexw(infotbl_W(2)),'h ');
        cmpstr:=wrhexw(infotbl_W(2));
        lookupdev(High(String),false);
        WriteLn;


        if not summary then
          begin
{ command register }
            Write(' Command ',wrhexw(infotbl_W(4)),'h');
            column_pos:=1+Length(' Command ')+4+1;
            column_pos_restart:=1+Length(' ')+2;
            first:=true;
            Write(' (');Inc(column_pos,2);
            if infotbl(4) and   1=1   then printstatus('I/O Access');
            if infotbl(4) and   2=2   then printstatus('Memory Access');
            if infotbl(4) and   3=0   then printstatus('Bus Access Disabled!!');
            if infotbl(4) and   4=4   then printstatus('BusMaster');
            if infotbl(4) and   8=8   then printstatus('Special Cycles');
            if infotbl(4) and  16=16  then printstatus('MemWrite+Invalidate');
            if infotbl(4) and  32=32  then printstatus('VGA Palette Snoop');
            if infotbl(4) and  64=64  then printstatus('Parity Error Response');
            if infotbl(4) and 128=128 then printstatus('Wait Cycles');

            if infotbl(5) and   1=1   then printstatus('System Errors');
            if infotbl(5) and   2=2   then printstatus('Back-To-Back Transactions');
            if infotbl(5) and   4=4   then printstatus('Interrupt Disable');

            Write(')');
            WriteLn;

{ status register }
            Write(' Status ');
            column_pos:=1+Length(' Status ');
            column_pos_restart:=1+Length(' ')+2;
            print_decodestatus(infotbl_W(6));
            WriteLn;

{ misc. general registers }
            WriteLn(' Revision ',wrhex(infotbl(8)),'h',
                    ', Header Type ',wrhex(infotbl($e)),'h',
                    ', Bus Latency Timer ',wrhex(infotbl($d)),'h');

{ header type 0 only : display latency and grant figures }
            if headertype=0 then
              begin
                if infotbl_W($3e)<>0 then
                  WriteLn(' Minimum Bus Grant ',wrhex(infotbl($3e)),'h, Maximum Bus Latency ',wrhex(infotbl($3f)),'h');
              end;

{ self test }
            Write(' Self test ',wrhex(infotbl($f)),'h (Self test ');
            if infotbl($f) and $80=0 then Write('not ');
            Write('supported');


            if infotbl($f) and $80=$80 then
              begin
                Write(': Completion code ',wrhexb(infotbl($f) and $f),'h - ');
                if infotbl($f) and $0f=0 then
                  WriteColor('OK',LightGreen)
                else
                  WriteColor('Failed!!',LightRed);
              end;
            WriteLn(')');

{ cache }
            if infotbl($c)<>0 then
              WriteLn(' Cache line size ',infotbl($c)*4,' Bytes (',infotbl($c),' DWords)');


{ class code stuff }
            Write(' PCI Class ');

            search_class   :=infotbl($b);
            search_subclass:=infotbl($a);
            search_progif  :=infotbl($9);

            { special progif encoding for Storage/IDE controller }
            if (search_class=1) and (search_subclass=1) then search_progif:=0;
            { special progif encoding for Bridge/RACEway }
            if (search_class=6) and (search_subclass=8) then search_progif:=search_progif and $01;

            if search_class=$ff then
              begin
                Write('FFh ');
                WriteColor('(does not meet any PCI-SIG defined class)',LightGreen);
              end

            else
            if not (search_class in [Low(PCI_class_names)..High(PCI_class_names)]) then
              WriteColor(wrhexb(search_class)+'h Unknown!',Yellow)

            else
              begin

                if search_class in [Low(PCI_class_names)..High(PCI_class_names)] then
                  WritePCColor(PCI_class_names[search_class],Yellow);

                Write(', type ');


                found:=false;

                for i:=Low(pci_class_array) to High(pci_class_array) do
                  begin
                    if  (pci_class_array[i].class_  =search_class   )
                    and (pci_class_array[i].subclass=search_subclass)
                    and (pci_class_array[i].progif  =search_progif  ) then
                      begin
                        found:=true;
                        WritePCColor(PCI_class_array[i].cname,Yellow);
                        Break;
                      end;
                  end;


                if not found then
                  begin
                    for i:=Low(pci_class_array) to High(pci_class_array) do
                      if  (pci_class_array[i].class_  =search_class   )
                      and (pci_class_array[i].subclass=search_subclass)
                      and (pci_class_array[i].progif  =0              ) then
                        begin
                          found:=true;
                          WritePCColor(PCI_class_array[i].cname,Yellow);
                          Write(', programming interface ');
                          WriteColor(wrhex(infotbl($9)),Yellow);
                          Break;
                        end;
                  end;


                if not found then
                  WriteColor('Unknown!',LightRed);

              end;


            WriteLn;
          end;



        if not summary then
          begin

{ look for generic PCI IDE controller & decode it's info, if present }
            if (infotbl($b)=01) and (infotbl($a)=01) then
              begin
                WriteLn(' PCI EIDE Controller Features :');
                Write  ('   BusMaster EIDE is ');
                if infotbl($9) and $80=0 then
                  WriteColor('NOT ',LightRed);
                WriteLn('supported');

                look_if_ide_channels_enabled;

                Write  ('   Primary   Channel is ');
                if infotbl($9) and 1=0 then
                  begin
                    if enabled_primary_ide then
                      begin
                        Write('at I/O Port 01F0h and IRQ 14');
                        if infotbl($3c)<>14 then Inc(irqmap[14]);
                      end
                    else
                      Write('disabled');
                  end
                else
                  Write('in native mode at Addresses 0 & 1');
                WriteLn;

                Write('   Secondary Channel is ');
                if infotbl($9) and 4=0 then
                  begin
                    if enabled_secondary_ide then
                      begin
                        Write('at I/O Port 0170h and IRQ 15');
                        if infotbl($3c)<>15 then Inc(irqmap[15]);
                      end
                    else
                      Write('disabled');
                  end
                else
                  Write('in native mode at Addresses 2 & 3');
                WriteLn;
              end;

          end
        else
          begin
{ summary mode: pick up IRQs only }
            if (infotbl($b)=01{Storage}) and (infotbl($a)=01{IDE}) then
              begin
                look_if_ide_channels_enabled;
                if (infotbl($9) and 1=0) and (infotbl($3c)<>14) and enabled_primary_ide   then Inc(irqmap[14]);
                if (infotbl($9) and 4=0) and (infotbl($3c)<>15) and enabled_secondary_ide then Inc(irqmap[15]);
              end;
          end;




{ if type 0 or 2 table & if Subsystem ID exists, display and scan file for match }
        ReadSubVendorDevice(sub_vendor,sub_device);

        if (sub_vendor<>0) or (sub_device<>0) then
          begin

{ subsystem ID }

            cmpstr:=wrhexw(sub_device)+wrhexw(sub_vendor);
            Write(' Subsystem ID ',cmpstr,'h');


            genssid:=    (sub_vendor=infotbl_W(0))
                     and (sub_device=infotbl_W(2));

            oemidnum:='';
            oemidstr:='';
            bogusid:=false;
            failed:=true;
{ use the line that was read for revchk, or the first O or X entry will be missed! }
            userev:=true;
            while not Eof2(f) do
              begin

                if userev and (revchk<>'') then vstr:=revchk
                else ReadLn2(f,vstr);
                userev:=false;

                if Length(vstr)<3 then vstr:=';   oops';

                case vstr[1] of
{ OEM Vendor ID }
                  'O':
                    if StrLComp(@vstr[3],@cmpstr[5],4)=0 then
                      begin
                        oemidstr:=Copy(vstr,8,Length(vstr)); { closest match }
                        oemidnum:=Copy(vstr,3,4); { matching vendor name }
                      end;

                  'S':
                    if StrLComp(@vstr[3],@cmpstr[1],4)=0 then
                      if oemidnum<>'' then
                        begin
                          oemidstr:=Copy(vstr,8,Length(vstr));
                          WriteColor(' '+oemidstr,Yellow);
                          if genssid then
                            WriteColor(' (Generic ID)',LightCyan);
                          WriteLn;
                          failed:=false;
                          Break;
                        end;


{ Oddball 8 digit entry }
                  'X':
                    if StrLComp(@vstr[3],@cmpstr[1],8)=0 then
                      begin
                        oemidnum:=Copy(vstr,7,4); { matching vendor name }
                        bogusid:=true;
                        WriteColor(' '+Copy(vstr,12,255),Yellow);
                        if genssid then
                          WriteColor(' (Generic ID)',LightCyan);
                        WriteLn;
                        failed:=false;
                        Break;
                      end;

                  { remember to ignore comment lines here also!!! }
                  ';':;

                else
                  { anything other than O X S or comment }
                  Break;

                end;

              end; { while not eof/not failed }


            if failed then
              begin { try to come up with guesses }

                if oemidstr<>'' then
                  begin
                    WriteColor(' '+oemidstr,Yellow);
                    WriteColor(' (Guess Only!)',White);
                  end
                else
                  WriteColor(' Unknown',LightRed);

                if genssid then
                  WriteColor(' (Generic ID)',LightCyan);

                WriteLn;

              end;


{ subsystem vendor }
            Write(' Subsystem Vendor ',wrhexw(sub_vendor),'h');

            if bogusid then
              begin
                WriteLnColor(' Known Bad Subsystem ID - no Vendor ID Available',White);
              end
            else
              begin
                if oemidnum<>'' then cmpstr:=oemidnum
                else cmpstr:=wrhexw(sub_vendor);

                Close2(f);            { get back to start of file, as the}
                Reset2(f);            { subsys vendor may be higher up...!}

                failed:=true;

                while not Eof2(f) do
                  begin
                    ReadLn2(f,vstr);
                    if Length(vstr)<3 then vstr:=';   oops';
                    if (vstr[1]='V') and (StrLComp(@vstr[3],@cmpstr[1],4)=0) then
                      begin
                        WriteLnColor(' '+Copy(vstr,8,255),Yellow);
                        failed:=false;
                        Break;
                      end;
                  end;
              end;

            if failed then
              begin
                WriteLnColor(' Unknown',LightRed);
              end;

          end; { subsystem id available }


{ always }
        Close2(f);



        if not summary then
          begin

{ type 0 header = 6 entries, type 1 = 2, type 2 = skip  }
            case headertype of
              0:pp:=6;
              1:pp:=1;
            else
                pp:=0;
            end;

            { pass 1: get number of used registers }
            nn:=0;
            ppused:=0;
            while nn<pp do
              begin
                addr:=infotbl_L($10+(nn*4));
                Inc(nn);
                if addr<>0 then
                  begin
                    if (addr and $7)=(0 shl 0)+(2 shl 1) then
                      begin { 64bit memory }
                        Inc(nn);
                      end;
                    ppused:=nn;
                  end;
              end;

            { pass 2: display registers }

            nn:=0;
            while nn<{pp}ppused do
              begin
                addr:=infotbl_L($10+(nn*4));
                if addr=0 then
                  begin
                    Write(' Address ',nn,' ');
                    WriteColor('is not present or not configured!',DarkGray)
                  end
                else
                  begin

                    Write(' Address ',nn,' is a');

                    if addr and 1=1 then
                      begin
                        Write('n I/O Port : ');
                        if (addr and $ffff0000)<>0 then
                          Write(wrhexl(addr and $fffffffc),'h')
                        else
                          Write(wrhexw(addr and $fffffffc),'h');
                        size:=ResourceSize(addr and $fffffffc,ResourceIO);
                        if size<>0 then
                          Write(chars_to_,wrhexw(addr and $fffffffc+(size-1)),'h');
                        if (addr and $fffffffc)=0 then
                          WriteColor(' (unconfigured)',DarkGray);
                      end
                    else
                      begin
                        Write(' Memory Address');
                        case (addr shr 1) and $3 of
                         {0:Write(' (anywhere in 0-4GiB');}
                          0:Write(' (0-4GiB');
                          1:Write(' (below 1MiB');
                         {2:Write(' (anywhere in 64-bit space');}
                          2:Write(' (64-bit');
                          3:Write(' (reserved');
                        end;
                        if addr and 8=8 then Write(', Prefetchable) : ')
                        else Write(') : ');

                        { 64 bit needs next addr }
                        if ((addr shr 1) and $3)=2 then
                          begin
                            addrhi:=infotbl_L($10+(nn*4)+4);
                            if addrhi<>0 then
                              Write(wrhexl(addrhi));
                            Inc(nn);
                          end
                        else
                          addrhi:=0;

                        Write(wrhexl(addr and $fffffff0)+'h');

                        { size the register }
                        if addrhi=0 then
                          begin
                            size:=ResourceSize(addr  and $fffffff0,ResourceMem);
                            if size<>0 then
                            Write(chars_to_,wrhexl(addr and $fffffff0+(size-1)),'h');
                          end;

                      end;

                  end;
                Inc(nn);
                WriteLn;
              end;

            case headertype of
              $00:
                begin {other}
                  addr:=infotbl_L($28);
                  if (addr<>0) and (addr<>-1) then
                    begin { Card Information Structure }
                      Write(' CIS can read from ');
                      case addr and 7 of
                        0:Write('device specific configuration space');
                     1..6:Write('Address ',addr and 7-1,' mapped memory');
                        7:Write('expansion ROM image ',addr shr 28);
                      end;
                      Write(', offset ');
                      addr:=addr and $0ffffff8; (* bit  27..3 *)
                      if addr<=$ff then
                        Write(wrhex(addr))
                      else
                      if addr<=$ffff then
                        Write(wrhexw(addr))
                      else
                        Write(wrhexl(addr));
                      WriteLn('h');
                    end;
                end;
              $02:
                begin {cardbus bridge}
                  {previously, we displayed socket address brdige windows}
                end;
            end;

          end;

{ all header types - list IRQ, if present }
        if (infotbl($3c) in [1..254]) or (infotbl($3d)<>0) then
          begin
            Write(' System IRQ ');
            if infotbl($3c) in [0,255] then
              WriteColor('(disabled)',DarkGray)
            else
              Write(infotbl($3c));
            Write(', INT# ');
            if infotbl($3d)=0 then WriteColor('-',DarkGray)
            else Write(Chr(infotbl($3d)+Ord('@')));
            WriteLn;
            Inc(irqmap[infotbl($3c)]);
            if infotbl($3c) in [16..254] then apic:=true;
          end;



        if not summary then
          begin

{ type 0,1 header only - List ExpROM, if present }
            case headertype of
              0:lb:=$30;
              1:lb:=$38;
            else
                lb:=0;
            end;

            if (lb<>0) and (not readonly) then
              begin
                org_rom_addr:=infotbl_L(lb);

                write_dword(infotbl_deviceid,infotbl_func,infotbl_bus,lb,$fffffffe);
                reset_infotbl_cache;

                { bit 31..11=start address,10..1=reserved 0=enable }

                { example:
                   Adaptec 2940:
                     compressed BIOS size(header)=$50*512=$0a000
                     -> address room                     =$10000
                     writing fffffffe results in ffff0000 }

                {romsize:=((not infotbl_L(lb)+1) shr 11) shl 1;}
                romresult:=infotbl_L(lb);
                write_dword(infotbl_deviceid,infotbl_func,infotbl_bus,lb,org_rom_addr);
                reset_infotbl_cache;

                if romresult<>0 then
                  begin
                    { find lowest 1-bit, gives expROM size }
                    romsize:=11;
                    while (not Odd(romresult shr (romsize))) and (romsize<32) do
                      Inc(romsize);
                    romsize:=1 shl (romsize-10); { in KiB }

                    Write(' Expansion ROM of ');
                    if romsize>=1024 then
                      Write(romsize shr 10,' MiB')
                    else
                      Write(romsize,' KiB');
                    Write(' decoded by this card, ');
                    if Odd(org_rom_addr) then
                      Write('at address ',wrhexl(org_rom_addr and $fffff800))
                    else
                      WriteColor('currently disabled',DarkGray);
                    WriteLn;
                  end;
              end;


          end; { summary }


{ PCI Bridges info starts here }

{ type 1 header only - List bridge info }
        if not summary then
          begin

            if headertype=1 then
              begin
                WriteLn(' PCI Bridge Information:');
                Write  ('   Primary Bus Number ');
                if dohtml and showtree then write_extra_html('<a href="#bus'+Int2Str(infotbl($18))+'">');
                WriteIntColor(infotbl($18),LightCyan);
                if dohtml and showtree then write_extra_html('</a>');
                Write  (', Secondary Bus Number ');

                if dohtml and showtree then
                  write_extra_html('<a href="#bus'+Int2Str(infotbl($19))+'">');
                WriteIntColor(infotbl($19),LightCyan);
                if dohtml and showtree then
                  write_extra_html('</a>');
                { zero is invalid (power on value), not 100% sure if smaller value
                   is not allowed. at least it is not typical }
                if infotbl($19)<=infotbl_bus then
                  WriteColor(' (invalid)',LightRed);
                Write(', Subordinate Bus number ');
                WriteIntColor(infotbl($1a),LightCyan);
                WriteLn;

{ secondary bus command }
                Write('   Secondary Bus Command ');
                column_pos:=1+Length('   Secondary Bus Command ');
                column_pos_restart:=1+Length('   ')+2;
                print_decodebridgecontrol(infotbl_W($3e),1);
                WriteLn;

{ secondary bus status }
                Write('   Secondary Bus Status  ');
                column_pos:=1+Length('   Secondary Bus Status  ');
                column_pos_restart:=1+Length('   ')+2;
                print_decodestatus(infotbl_W($1e));
                WriteLn;

{ latency }
                WriteLn('   Secondary Bus Latency ',wrhex(infotbl($1b)),'h');

{ I/O port range passed by bridge }
                if infotbl_W($1c)<>0 then
                  begin
                    Write('   I/O Port Range Passed to Secondary Bus : ');

                    case infotbl($1c) and $f of
{ 16-bit I/O }
                      0:
                        begin
                          if infotbl($1d)<infotbl($1c) then
                            Write('None')
                          else
                            Write(wrhexb(infotbl($1c) shr 4),'000h'+chars_to_,
                                  wrhexb(infotbl($1d) shr 4),'FFFh (16-bit)');
                        end;

{ 32-bit I/O }
                      1:
                        begin
                          if infotbl_W($32) < infotbl_W($30) then
                            Write('None')
                          else
                            Write(wrhexw(infotbl_W($30)),wrhexb(infotbl($1c) shr 4),'000h'+chars_to_,
                                  wrhexw(infotbl_W($32)),wrhexb(infotbl($1d) shr 4),'FFFh (32-bit)');
                        end;
                    else
                      Write('?');
                    end;
                    WriteLn;
                  end;


{ memory range passed by bridge }
                Write('   Memory   Range Passed to Secondary Bus : ');
                mb:=infotbl_W($20) and $fff0;
                ml:=infotbl_W($22) or  $000f;
                if ml<mb then
                  Write('None')
                else
                  Write(wrhexw(mb),'0000h'+chars_to_,wrhexw(ml),'FFFFh');
                WriteLn;



 { optional: prefetchable memory range passed by bridge }
                if infotbl_L($24)<>0 then
                  begin
                    Write('   Prefetchable Memory Range Passed to Secondary Bus : ');

                    mb:=infotbl_W($24) and $fff0;
                    ml:=infotbl_W($26) or  $000f;

                    if ml<mb then
                      Write('None')
                    else
                      begin

{ 64 bit base reg? }
                        if infotbl($24) and $0f=1 then mbh:=infotbl_L($28) else mbh:=0;
{ 64 bits limit reg? }
                        if infotbl($26) and $0f=1 then mlh:=infotbl_L($2c) else mlh:=0;

                        if mlh<mbh then
                          Write('None')
                        else
                          if (mbh=0) and (mlh=0) then
                            Write(            wrhexw(mb),'0000h'+chars_to_,
                                              wrhexw(ml),'FFFFh')
                          else
                            Write(wrhexl(mbh),wrhexw(mb),'0000h..'+chars_to_,
                                  wrhexl(mlh),wrhexw(ml),'FFFFh')
                      end;
                    WriteLn;
                  end;


              end; { header type 1 }

          end; { not summary }



{ type 2 header only - List bridge info }

        if not summary then
          begin
            if headertype=2 then
              begin
                WriteLn(' CardBus Bridge Information:');
                Write  ('   PCI Bus Number ');
                if dohtml and showtree then write_extra_html('<a href="#bus'+Int2Str(infotbl($18))+'">');
                WriteIntColor(infotbl($18),LightCyan);
                if dohtml and showtree then write_extra_html('</a>');
                Write(', CardBus Bus Number ');
                if dohtml and showtree then
                  write_extra_html('<a href="#bus'+Int2Str(infotbl($19))+'">');
                WriteIntColor(infotbl($19),LightCyan);
                if dohtml and showtree then
                  write_extra_html('</a>');

                if  (infotbl($19)<>0)
                and (bustype[infotbl($19)]<>bustype_CardBus)
                and (bustype[infotbl($19)]<>bustype_PCI_known) then
                  begin
                    case bustype[infotbl($19)] of
                      bustype_PCI_known,
                      bustype_None:
                    else
                      WriteColor(' (is already '+bustype_names[bustype[infotbl($19)]]+' bus)',LightRed);
                    end;
                  end;

                Write  (', Subordinate Bus number ');
                WriteIntColor(infotbl($1a),LightCyan);
                WriteLn;

                WriteLn('   CardBus Latency ',wrhex(infotbl($1b)),'h');

                Write('   Secondary Bus Command ');
                column_pos:=1+Length('   Secondary Bus Command ');
                column_pos_restart:=1+Length('   ')+2;
                print_decodebridgecontrol(infotbl_W($3e),2);
                WriteLn;

                Write('   Secondary Bus Status  ');
                column_pos:=1+Length('   Secondary Bus Status  ');
                column_pos_restart:=1+Length('   ')+2;
                print_decodestatus(infotbl_W($16));
                WriteLn;

                addr:=infotbl_L($10);
                Write('   Socket/ExCa Base Memory Address : ',wrhexl(addr),'h');
                size:=ResourceSize(addr,ResourceMem);
                if size<>0 then
                  Write(chars_to_,wrhexl(addr+(size-1)),'h');
                WriteLn;
                show_address_range(0,infotbl_L($1c),infotbl_L($20),false,Odd(infotbl_W($3e) shr 8));
                show_address_range(1,infotbl_L($24),infotbl_L($28),false,Odd(infotbl_W($3e) shr 9));
                show_address_range(0,infotbl_L($2c),infotbl_L($30),true ,false);
                show_address_range(1,infotbl_L($34),infotbl_L($38),true ,false);
              end;
          end;


{ explore the capabilities list, if present }
        if not summary then
          begin
            if (infotbl(6) and $10=$10) then docapdecode;
          end;


{ do a hex-dump, if requested }
        if dumpregs then
          begin
            WriteLn;
            WriteLn(' Hex-Dump of device configuration space follows:');
            for i:=0 to 15 do
              begin
                Write('  ',wrhexw(i*16),'  ');
                for j:=i*16 to i*16+15 do
                  Write(wrhex(infotbl(j)),' ');
                Write('   ');
                for j:=i*16 to i*16+15 do
                  if infotbl(j)<32 then
                    Write('.')
                  else
                    Write(Chr(infotbl(j)));
                WriteLn;
              end;
          end;

        WriteLn;
      end; { not installermode }

  end; { showallinfo }



procedure search_file(var filefound:string;const fname,pathvar:PChar);
  var
    filefound_p         :array[0..260] of char;
    pp                  :pChar;
  begin
    pp:=WinDos.GetEnvVar(pathvar);
    if pp=nil then
      filefound:=''
    else
      begin
        WinDos.FileSearch(filefound_p,fname,pp);
        filefound:=StrPas(filefound_p);
      end;
  end;

function no_routing_table_entry(const secondary_bus:byte):boolean;
  var
    i                   :word;
  begin
    for i:=0 to (len shr 4)-1 do
      with irqbuffR.IRQ_routing_table_entry_Array[i] do
        if PCI_bus_number=secondary_bus then
          begin
            no_routing_table_entry:=false;
            Exit;
          end;

    no_routing_table_entry:=true;
  end;

type
  link_use_4_type       =array[1..4] of ^link_use_type;
  column_use_4_type     =array[1..4] of byte;

procedure show_irqpin_use(const bus_from,bus_to,device_from,device_to:byte;
                          const link_use:link_use_4_type;
                          const column_use:column_use_4_type;
                          var device_detected,pins_used:boolean);
  var
    b,d,f,
    sl,
    i,
    org_bus,
    org_dev,
    org_func            :word;
    better_information  :boolean;
    next_link_use       :link_use_4_type;
    next_column_use     :column_use_4_type;

  begin
    org_bus:=infotbl_bus;
    org_dev:=infotbl_deviceid;
    org_func:=infotbl_func;

    for b:=bus_from to bus_from do {bus_to will be found by bridges}
      for d:=device_from to device_to do
        for f:=0 to 7 do
          begin
            set_lookup_device(d,f,b);
            if infotbl_W($00)<>$ffff then
              begin
                device_detected:=true;

                { PCI IDE IRQ 15/14 }
                if (infotbl($b)=01) and (infotbl($a)=01) and ((infotbl($9) and $5)=0) then
                  { no pin? }
                else
                  if infotbl($3d) in [1..4] then
                    begin
                      if not pins_used then
                        begin
                          WriteLn;
                          pins_used:=true;
                        end;
                      Write('          IRQ:','':2+({infotbl($3d)}column_use[1+(infotbl($3d)-1+d-device_from) and 3]-1)*5);
                      Write('',infotbl($3c):3,'');
                      Write('':(4-{infotbl($3d)}column_use[1+(infotbl($3d)-1+d-device_from) and 3])*5+2);
                      cmpstr:=wrhexw(infotbl_W(0));
                      lookupven(true);
                      cmpstr:=wrhexw(infotbl_W(2));
                      lookupdev(numbers_of_screen_columns-38,true);
                      WriteLn;
                      if infotbl($3d) in [1..4] then
                        begin
                          i:=1+(infotbl($3d)-1+d-device_from) and 3;
                          link_use[i]^:=link_use[i]^+[infotbl($3c)];
                        end;
                    end;

                if is_bridge_with_pass_pci_irq then
                  { PCI bridge - pass through pin INTA#..INTD# }
                  begin

                    { look if ther is an routing table entry for an device
                       that is behind this bridge. if it is, do not recurivly
                       call ourself, avoid multiple findings..
                       ..assume that an more specific entry is correct }
                    better_information:=false;
                    for sl:=0 to (len shr 4)-1 do
                      with irqbuffR.IRQ_routing_table_entry_Array[sl] do
                        if PCI_bus_number=infotbl($19) then
                          begin
                            better_information:=true;
                            Break;
                          end;



                    if (not pins_used) and (f=0) and (not is_multifunction) then
                      { singe function bridge, does not use an IRQ itself
                         print it on the same line as the link values
                         example: AGP bridge }
                      pins_used:=true
                    else
                      begin
                        if not pins_used then
                          begin
                            WriteLn;
                            pins_used:=true;
                          end;
                        case infotbl($3d) of
                          1:Write('          Bridge **                 ');
                          2:Write('          Bridge      **            ');
                          3:Write('          Bridge           **       ');
                          4:Write('          Bridge                **  ');
                        else
                            Write('          Bridge **   **   **   **  ');
                        end;
                      end;

                    cmpstr:=wrhexw(infotbl_W(0));
                    lookupven(true);
                    cmpstr:=wrhexw(infotbl_W(2));
                    lookupdev(numbers_of_screen_columns-38,true);
                    WriteLn;

                    { show connected devices.. if enabled }
                    if  (not better_information)
                    and (f<infotbl($19))             { current<secondaray }
                    and (infotbl($19)<=infotbl($1a)) { secondary<=subordinate }
                    and (infotbl($1a)<=bus_to)       { subordinate<=limit }
                     then
                      if no_routing_table_entry(infotbl($19)) then
                        begin

                          if infotbl($3d) in [1..4] then
                            begin
                              next_link_use[1]:=link_use[1+(infotbl($3d)-1+d-device_from) and 3];
                              next_link_use[2]:=next_link_use[1];
                              next_link_use[3]:=next_link_use[1];
                              next_link_use[4]:=next_link_use[1];
                              next_column_use[1]:=column_use[1+(infotbl($3d)-1+d-device_from) and 3];
                              next_column_use[2]:=next_column_use[1];
                              next_column_use[3]:=next_column_use[1];
                              next_column_use[4]:=next_column_use[1];
                            end
                          else
                            for i:=1 to 4 do
                              begin
                                next_link_use  [i]:=link_use  [1+(i-1+d-device_from) and 3];
                                next_column_use[i]:=column_use[1+(i-1+d-device_from) and 3];
                              end;

                          show_irqpin_use(infotbl($19),infotbl($1a),0,31,
                                          next_link_use,next_column_use,
                                          device_detected,pins_used);
                        end;
                  end

              end;



            if not is_multifunction then Break;
          end;
    { restore current searched device }
    set_lookup_device(org_dev,org_func,org_bus);
  end;

{ final summarial IRQ info }
procedure do_irqsummary;
  var
    i                   : word;
    disp                : byte;

  begin
    WriteLn;

    if dohtml then
      writeln_extra_html('<a name="irqsummary"></a>');

    Write('IRQ Summary: ');
    failed:=true;
    disp:=0;
    irqmap[  0]:=0;
    irqmap[255]:=0;
    for i:=Low(irqmap) to High(irqmap) do if irqmap[i]>0 then Inc(disp); { count IRQs}
    for i:=Low(irqmap) to High(irqmap) do if irqmap[i]>0 then
      begin
        if failed then
          begin
            if disp=1 then Write('IRQ ')
            else           Write('IRQs ');
          end
        else
          Write(',');
        Write(i);
        failed:=false;
      end;
    if failed then WriteLn('No IRQ''s are used by PCI Devices!')
    else
      begin
        if disp=1 then Write(' is') else Write(' are');
        WriteLn(' used by PCI devices');
      end;

    Write('Shared IRQs: ');
    failed:=true;
    for i:=Low(irqmap) to High(irqmap) do if irqmap[i]>1 then
      begin
        if not failed then Write('             ');
        WriteLn('IRQ ',i,' is shared by ',irqmap[i],' PCI Devices');
        failed:=false;
      end;
    if failed then WriteLn('There are no shared PCI IRQs');
    if apic then
      begin
        WriteLn;
        WriteLn('IRQ control is currently managed by the system APIC controller - IRQ info is');
        WriteLn('not actual hardware settings...');
      end;

  end; { do_irqsummary }

procedure WriteRLG(const s:string);
  var
    tmp               :string;
    i                 :word;
  begin
    { line graphics depend on current codepage..                     }
    { - on Linux, this is completely unreliable                      }
    { - on Windows, this may need updates for GUI                    }
    { - Works fine for OS/2 and DOS codpages 437,850 and similar.    }
    { when redirected to printer, the printer has to configured..    }
    tmp:=s;
    if force7bittree then
      for i:=1 to Length(tmp) do
        case tmp[i] of
          'Ä':tmp[i]:='-';
          'À':tmp[i]:='`';{'\'?}
          'Ã':tmp[i]:='+';
          '³':tmp[i]:='|';
         #$10:tmp[i]:='>';
         #$11:tmp[i]:='<';
        end;

    { keep #$10 only when it is later converted to unicde $25ba }
    if not dohtml then
      for i:=1 to Length(tmp) do
        case tmp[i] of
         #$10:tmp[i]:='>';
         #$11:tmp[i]:='<';
        end;

    Write(tmp);
  end;

procedure do_showtree;
  var
    olb,
    i,j                 :word;
    more_devices_on_this_bus,
    more_subfunctions_on_this_device:boolean;
    found               :boolean;

    {$IfDef show_classcode_tree}
    search_class,
    search_subclass,
    search_progif       :byte;
    {$EndIf show_classcode_tree}


  begin


    WriteLn;
    WriteLn;

    if dohtml then
      writeln_extra_html('<a name="showtree"></a>');

    writeln('PCI Busses, Devices and Device Functions Tree');

    olb:=$ff;

    i:=1-1;
    while i<tree_used do
      begin
        Inc(i);

        set_lookup_device(tree[i].d,tree[i].f,tree[i].b);

        if tree[i].b<>olb then
          begin
            WriteLn;

            if dohtml then
              write_extra_html('<a name="bus'+Int2Str(tree[i].b)+'"></a>');

            WriteRLG('ÄÄBus ');
            WriteIntColor(tree[i].b,LightCyan);

            if behind_secondary[tree[i].b] and (tree[i].vid=$ffff) and (tree[i].did=$ffff) then
              begin

                j:=0;
                while (i+j<tree_used) do
                  if behind_secondary[tree[i+j+1].b] and (tree[i+j+1].vid=$ffff) and (tree[i+j+1].did=$ffff) then
                    Inc(j)
                  else
                    Break;

                if j>0 then
                  begin
                    Write('..');
                    WriteIntColor(tree[i+j].b,LightCyan);
                  end;

                WriteLn(' : Not Currently Present/Configured');
                Inc(i,j);
                Continue;
              end;

            WriteLn(' (',bustype_names[bustype[tree[i].b]],')');     { bus }
          end;


        more_devices_on_this_bus:=false;
        for j:=i+1 to tree_used do
          begin
            if tree[j].b<>tree[i].b then Break; { on other bus }
            if tree[j].d=tree[i].d then Continue; { still same device }
            more_devices_on_this_bus:=true;
            Break;
          end;

        more_subfunctions_on_this_device:=false;
        for j:=i+1 to tree_used do
          begin
            if tree[j].b<>tree[i].b then Break; { on other bus }
            if tree[j].d<>tree[i].d then Continue; { on other device }
            more_subfunctions_on_this_device:=true;
            Break;
          end;

        if more_devices_on_this_bus then
          if tree[i].f=0 then
            WriteRLG('   ÃÄÄ')
          else
            WriteRLG('   ³  ')
        else
          if tree[i].f=0 then
            WriteRLG('   ÀÄÄ')
          else
            WriteRLG('      ');


        if (tree[i].vid=$ffff) and (tree[i].did=$ffff) then
          begin
            WriteLn(' No Devices Currently Present');
            Continue;
          end;


{ up to here colums before "device" }
{ after here colums for func }



        if tree[i].f=0 then
          begin

            if dohtml and (not more_subfunctions_on_this_device) then
              write_extra_html('<a href="#bus'+Int2Str(tree[i].b)
                                       +'_dev'+Int2Str(tree[i].d)
                                      +'_func'+Int2Str(tree[i].f)
                                      +'">');


            Write(' Device ',tree[i].d:2);              { device }

            if dohtml and (not more_subfunctions_on_this_device) then
              write_extra_html('</a>');

          end;

        if (tree[i].f=0) and more_subfunctions_on_this_device then
          begin
            Writeln;                                    { MF func 0 only }
            if more_devices_on_this_bus then
              WriteRLG('   ³  ')
            else
              WriteRLG('      ');
          end;

        if (tree[i].f>0) or more_subfunctions_on_this_device then
          begin
            if more_subfunctions_on_this_device then
              WriteRLG('   ÃÄÄ')
            else
              WriteRLG('   ÀÄÄ');

            if dohtml then
              write_extra_html('<a href="#bus'+Int2Str(tree[i].b)
                                       +'_dev'+Int2Str(tree[i].d)
                                      +'_func'+Int2Str(tree[i].f)
                                      +'">');

            Write(' Function ',tree[i].f);        { func bar }

            if dohtml then
              write_extra_html('</a>');

          end;

        Write(' - ',wrhexw(tree[i].vid),'h:',wrhexw(tree[i].did),'h  ');

        {$IfDef show_devicenames_tree}

        cmpstr:=wrhexw(tree[i].vid);
        lookupven(true);
        cmpstr:=wrhexw(tree[i].did);
        if (tree[i].f>0) or more_subfunctions_on_this_device then
          lookupdev(numbers_of_screen_columns-41,true)
        else
          lookupdev(numbers_of_screen_columns-34,true);
        {$EndIf show_devicenames_tree}
        {$IfDef show_classcode_tree}
{ display device class info }

        search_class   :=infotbl($b);
        search_subclass:=infotbl($a);
        search_progif  :=infotbl($9);

        { special progif encoding for Storage/IDE controller }
        if (search_class=1) and (search_subclass=1) then search_progif:=0;
        { special progif encoding for Bridge/RACEway }
        if (search_class=6) and (search_subclass=8) then search_progif:=search_progif and $01;

        found:=false;
        for j:=Low(pci_class_array) to High(pci_class_array) do
          with pci_class_array[j] do
            if  (class_  =search_class   )
            and (subclass=search_subclass)
            and (progif  =search_progif  ) then
              begin
                Write(cname);
                found:=true;
                Break;
              end;

        { would find UHCI for EHCI, if EHCI is not listed in table.. }
        if not found then
          for j:=Low(pci_class_array) to High(pci_class_array) do
            with pci_class_array[j] do
              if  (class_  =search_class   )
              and (subclass=search_subclass)
              and (progif  =0              ) then
                begin
                  Write(cname);
                  found:=true;
                  Break;
                end;

        if not found then
          Write('Unknown');

        if (Low(pci_class_names)<=search_class) and (search_class<=High(pci_class_names)) then
          Write(' ',pci_class_names[search_class]);

        {$EndIf show_classcode_tree}

        WriteLn;

{ show outgoing buses }
        if headertype in [1,2] then
          if infotbl($19)>tree[i].b then
            if infotbl($19)<=infotbl($1a) then
              begin

                if more_devices_on_this_bus then
                  WriteRLG('   ³  ')
                else
                  WriteRLG('      ');

                if (tree[i].f>0) or more_subfunctions_on_this_device then
                  if more_subfunctions_on_this_device then
                    WriteRLG('   ³  ')
                  else
                    WriteRLG('      ');

                WriteRLG('   ÀÄÄ');

                WriteRLG('> '); (* #$10' ' makes not look better *)

                if dohtml then
                  write_extra_html('<a href="#bus'+Int2Str(infotbl($19))+'">');

                Write('Bus ');
                WriteIntColor(infotbl($19),LightCyan);

                if dohtml then
                  write_extra_html('</a>');

                if infotbl($19)<infotbl($1a) then
                  begin
                    Write('..');
                    WriteIntColor(infotbl($1a),LightCyan);
                  end;
                WriteLn;
              end;

        olb:=tree[i].b;
      end;
    WriteLn;
    WriteLn;
    WriteLn('A total of ',tree_used-tree_used_empty,' Items Found');
    WriteLn;
    WriteLn;
  end; { do_showtree }

var
  device_detected,
  pins_used     : boolean;
  sfunction     : word;
  count         : word;
  h             : byte;
  link_use      : linkuse_array_type;
  link_use_4    : link_use_4_type;
  column_use_4  : column_use_4_type;
  irq_usage_documented: array[0..31] of boolean;
  deviceid,
  func,
  bus           : byte;
  i,j           : word;
  cap_ptr       : byte;
  gotone        : boolean;
  os_version_string,
  hostname_string : string;

begin
  TextColor(LightGray);
  TextBackground(Black);

  open_pci_access_driver;

{ initialise important variables }
  {$IfDef pci_Debug}
  readonly:=true;
  {$Else}
  readonly:=false;
  {$EndIf}
  showhelp:=false;
  businfo:={false}true;
  dorouting:=true;
  dopcirouting:=false;
  dumpregs:=false;
  usebios:=true;
  summary:=false;
  installermode:=false;
  showtree:=false;
  {$IfDef VirtualPascal}
  vga50:=SysCtrlSelfAppType<2;
  {$Else}
  vga50:=true;
  {$EndIf}
  force7bittree:=false;
  dohtml:=false;

{ the following hack permits MS-DOS display output redirection to work }
  if ioredirected then
    begin
      Assign(output,'');
      Rewrite(output);
      numbers_of_screen_columns:=80;
    end
  else
{ code to do page pausing }
    begin
      ClrScr;
      install_pager;
      numbers_of_screen_columns:=Lo(WindMax)+1;
    end;



  for i:=Low(irqmap) to High(irqmap) do irqmap[i]:=0;
  failed:=true;


  if ParamCount>0 then
    for i:=1 to ParamCount do
      begin
        cmdstr:=ParamStr(i);
        while cmdstr<>'' do
          if cmdstr[1] in [' ',#9] then Delete(cmdstr,1,1) else Break;
        while cmdstr<>'' do
          if cmdstr[Length(cmdstr)] in [' ',#9] then Delete(cmdstr,Length(cmdstr),1) else Break;
        for j:=1 to Length(cmdstr) do
          cmdstr[j]:=UpCase(cmdstr[j]);
        if (Length(cmdstr)=Length('-?')) and (cmdstr[1] in ['+','-','/']) then
          case cmdstr[2] of
            'H':usebios:=false;
            'D':dumpregs:=true;
            'S':summary:=true;
            'T':dorouting:=false;
            'P':dopcirouting:=true;
            'B':businfo:=true; { businfo now on by default }
            'I':installermode:=true;
            '5':vga50:=false;
            '7':force7bittree:=true;
            'N':readonly:=true;
            'R':showtree:=true;
            'M':dohtml:=true;
            '?':showhelp:=true;
            'Z':(* do nothing -- debug:=true for pci32 1.2 *);
          else
                Write('Unknown Switch "',ParamStr(i),'", press Enter for help');
                ReadLn;
                showhelp:=true;
          end
        else
          begin
            Write('Unknown Command line parameter: "',ParamStr(i),'", press Enter for help');
            ReadLn;
            showhelp:=true;
          end;

        if showhelp then
          begin
            if vga50 then
              TextMode(Co80);
            WriteLn(' Help for PCI  (Version ',revision,')');
            TextColor(DarkGray);
            WriteRLG('ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ');WriteLn;
            TextColor(LightGray);
            WriteLn;
            WriteLn('Usage: PCI [-Option ..]      [] indicates optional parameter');
            WriteLn;
            WriteLn('-H : Use direct hardware access (instead of the BIOS) to retrieve PCI Info');
            WriteLn('     May be required for accurate reporting on Intel 430FX chipset+Award BIOS');
            WriteLn('-D : Do a hex-dump of each device''s entire configuration space');
            WriteLn('-S : Create a brief, summary report only; only devices and IRQs listed');
            WriteLn('-T : Disable the test ROM IRQ Routing Table function');
           {obsolte: option is forced to be true since 1.03..
            WriteLn('-B : Enable display of the Bus, Device & Function information');}
            WriteLn('-P : Enable display of PCI slot routing data');
            WriteLn('-I : Installer mode: produce raw data dump (for use with auto-setup programs)');
            WriteLn('-R : Draw a Tree of Busses, Devices and Device Functions');
            WriteLn('-? : Displays this help screen!');
            WriteLn('-N : Do not write to registers (PM readout, ExpRom size)');
            WriteLn('-5 : keep textmode');
            WriteLn('-7 : Display tree using ASCII chars');
            WriteLn('-M : Output HTML');
            WriteLn;
            WriteLn('PCI Supports generating reports to a file or printer using MS-DOS pipes; i.e.');
            WriteLn;
            WriteLn('  PCI -D > REPORT.TXT  (Save report to file),  PCI > LPT1:  (Print report)');
            WriteLn;
            WriteLn('PCI is written by Craig Hart, and is released as freeware, with no restictions');
            Write('on use or copying. Visit ');
            WriteColor('http://members.datafast.net.au/dft0802',LightCyan);
            WriteLn(' for updates to');
            WriteLn('the program and the PCI Database file PCIDEVS.TXT');
            Write('Look at ');
            WriteColor('http://kannegieser.net/veit',LightCyan);
            WriteLn(' for updates the *vk program variants.');
            Halt(10);
          end;
      end;

  if dohtml then
    begin
      os_version_string:='';
      { DOS/DPMI does not have a driver for PCI, so there is no need to
        display a DOS version number or DPMI server version }
      {$IfNDef MSDOS}
      {$IfNDef DPMI}
      {$IfNDef DPMI32}
      os_version_string:=' '+Int2Str(Lo(SysOsVersion))+'.'+Int2Str(Hi(SysOsVersion));
      {$EndIf}
      {$EndIf}
      {$EndIf}

      hostname_string:='';
      if GetEnv('hostname')<>'' then
        hostname_string:=' on machine '+GetEnv('hostname');

      Assign(output,'');
      Rewrite(output);
      install_html_ouput('PCI output ('
        +revision+' '
        {$IfDef VirtualPascal}
        +SysPlatformName
        {$Else}
        +'DOS'
        {$EndIf}
        +os_version_string
        +')'+hostname_string);

      writeln_extra_html('<link href="http://members.datafast.net.au/dft0802" '
                        +'rel=bookmark title="Craig Hart - original author, database">');
      writeln_extra_html('<link href="http://kannegieser.net/veit" '
                        +'rel=bookmark title="Veit Kannegieser - more platforms, customisations">');
      writeln_extra_html('<link href="./" rel=Up>');

      numbers_of_screen_columns:=255;
    end;

  {$IfDef Linux}
  force7bittree:=force7bittree or (not dohtml);
  {$EndIf}

{ fix up conflicting commandline switches }

  if installermode then
    begin
      dorouting:=false;
      dopcirouting:=false;
      dumpregs:=false;
      businfo:=false;
      summary:=false;
      vga50:=false;
    end;

  if summary then
    begin
      dumpregs:=false;
      dopcirouting:=false;
      dorouting:=false;
    end;


  if dohtml then
    begin
      writeln_extra_html('<link href="#main" rel=Chapter title="Main Device Decode">');

      if dopcirouting then
        begin
          writeln_extra_html('<link href="#pcirouting" rel=Chapter title="PCI slot IRQ mapping information">');
          writeln_extra_html('<link href="#slot_int_map" rel=Chapter title="PCI slot INTx to IRQ-router mappings">');
        end;

      if dorouting then
        writeln_extra_html('<link href="#romrouting" rel=Chapter title="ROM PCI IRQ routing table Tests">');

      if not installermode then
        writeln_extra_html('<link href="#irqsummary" rel=Chapter title="IRQ Summary">');

      { bus/dev/func tree }
      if showtree and not installermode then
        writeln_extra_html('<link href="#showtree" rel=Chapter title="PCI Busses, Devices and Device Functions Tree">');

    end; (* dohtml header stuff *)


  if not ioredirected then
    if vga50 then
      TextMode(Co80+Font8x8);


  if dohtml then
    begin_body_html;


  if not installermode then
    begin

      WriteLn('Craig Hart''s PCI+AGP bus sniffer, Version ',revision,', freeware made in 1996-2005.');

      { calculate datafile searchpath: exefile path,... }
      pcidevs_path:=ParamStr(0);
      while (not (pcidevs_path[Length(pcidevs_path)] in ['\','/'])) and (pcidevs_path<>'') do
        Dec(pcidevs_path[0]);

      pcidevs_txt:=FSearch('pcidevs.txt',pcidevs_path);

      if pcidevs_txt='' then
        search_file(pcidevs_txt,'pcidevs.txt','PATH');

      if pcidevs_txt='' then
        search_file(pcidevs_txt,'pcidevs.txt','DPATH');


      if pcidevs_txt<>'' then
        begin
          Assign2(f,pcidevs_txt);
          {$i-}
          Reset2(f);
          {$i+}
        end;

      if (IOResult<>0) or (pcidevs_txt='') then
        begin
          WriteLn('PCI Halted:');
          WriteLn;
          WriteLn('Sorry, I cannot locate my PCIDEVS.TXT datafile!!!');
          Halt(10);
        end;
      Close2(f);
      {$i+}
    end;



  if test8086<2 then
    begin
      WriteLn('PCI Halted:');
      WriteLn;
      WriteLn('The PC Must be at least a 386 to possibly have a PCI or AGP bus!');
      Halt(1);
    end;

{ Look for PCI BIOS }
  pci_present_test;

  if failed then
    begin
      WriteLn('PCI Halted:');
      WriteLn;
      {$IfDef VirtualPascal}
      WriteLn('No PCI BIOS was detected!');
      {$Else} { DOS .. }
      WriteLn('No PCI BIOS was detected! (NB: I don''t work under Windows NT/2000/XP/2003 etc!)');
      WriteLn;
      WriteLn('For PCI reports under Win NT-era OS''s such as NT/2K/XP/2K3, use PCI_W.EXE');
      WriteLn;
      {$EndIf}
      Halt(2);
    end;



{ OK, we have PCI... do our stuff.. }


  if not installermode then
    begin

      (*WriteLn(' Craig Hart''s PCI+AGP bus sniffer, version ',revision,', freeware made in 1996-2005.');*)
      WriteLn;
      Write('PCI BIOS Version ',PCIverhi,'.',wrhex(PCIverlo),' found!');

      if summary then
        Write('                                  (Summary Report)');

      WriteLn;
      WriteLn('Number of PCI Busses : ',PCI_hibus+1);
      Write('PCI Characteristics  : ');
      if PCIchar and  1=1  then Write('Config Mechanism 1 ')
      else usebios:=true; { must use BIOS if no cfg mech 1 supported }
      if PCIchar and  2=2  then Write('Config Mechanism 2 ');
      if PCIchar and 16=16 then Write('Special Cycle Mechanism 1 ');
      if PCIchar and 32=32 then Write('Special Cycle Mechanism 2 ');
      WriteLn;
      WriteLn;
      Write('Searching for PCI Devices using ');
      {$IfDef OS2}
      if usebios then
        WriteLn('the OEMHLP$ driver')
      else
        WriteLn('Configuration Mechanism 1');
      {$EndIf}

      {$IfDef Win32}
      usebios:=true;
      case SysPlatformID of
        0,1:WriteLn('Configuration Mechanism 1')
      else
            WriteLn('Configuration Mechanism 1 [NT/2K/XP/etc Method]')
      end;
      {$EndIf}

      {$IfDef Linux}
      usebios:=true;
      WriteLn('the /proc/bus/pci filesystem');
      {$EndIf}

      {$IfNDef Linux_OS2_Win32}
      if usebios then { DOS, DPMI32 }
        WriteLn('the System BIOS')
      else
        WriteLn('Configuration Mechanism 1');
      {$EndIf}

      {$IfDef Os2}
      {WriteLn('Determining Resource size using RESMGR$ driver');}
      {$EndIf}

      WriteLn;

    end; { not installermode }


  if dohtml then
    writeln_extra_html('<a name="main"></a>');

  FillChar(tree,SizeOf(tree),0);
  tree_used:=0;
  tree_used_empty:=0;

  for bus:=Low(bustype) to High(bustype) do
    if bus<=pci_hibus then
      bustype[bus]:=bustype_PCI_known   { fix bugs for 440LX chipset, 2 PCI buses, AGP=1 bus! ?}
    else
      bustype[bus]:=bustype_None;

  FillChar(behind_secondary,SizeOf(behind_secondary),false);


  for bus:=Low(bustype) to High(bustype) do

    if (bustype[bus]<>bustype_None) or (bus<=pci_hibus) then
      begin

        gotone:=false;

        {$IfDef Debug}
        WriteLn('Examining bus ',bus);
        {$EndIf Debug}

        determine_pci_or_pci_express(bus);

        for deviceid:=0 to $1f do

          for func:=0 to 7 do
            begin

              set_lookup_device(deviceid,func,bus);
{don't try to read cfg-space of non-existant devices: hangs some chipsets!}
              if infotbl_W(0)<>$ffff then
                begin

                  gotone:=true;

{ remember CardBus stuff for later; skip if far bus=0 (i.e. unconfigured) }
                  if (infotbl($19)>bus) and (infotbl($19)<=infotbl($1a)) then (* bus<>0 and not already known bus *)
                    case headertype of
                      { 0:other }
                      1: { PCI-PCI (+AGP) }
                        begin

                          for i:=infotbl($19) to infotbl($1a) do
                            behind_secondary[i]:=i>infotbl($19);

                          cap_ptr:=first_cap_ptr;
                          while cap_ptr_valid(cap_ptr) do
                            begin
                              if infotbl(cap_ptr)=07 then (* pci-x msi *)
                                begin
                                  bustype[infotbl($19)]:=bustype_PCI_X;
                                  Break;
                                end;
                              cap_ptr:=infotbl(cap_ptr+1);
                            end;

                          { maybe look for "AGP" in the database entry ? }
                          if bustype[infotbl($19)] in [bustype_PCI_known,
                                                       bustype_PCI,
                                                       bustype_None] then
                            begin { VGA Mapping flag }
                              if (bus=0) and ((infotbl($3e) and $08)=$08) then
                                bustype[infotbl($19)]:=bustype_AGP
                              else
                                bustype[infotbl($19)]:=bustype_PCI; (* checks PCI Express later *)
                            end;
                          pci_hibus:=Max(pci_hibus,infotbl($19)); { secondary bus   }
                          pci_hibus:=Max(pci_hibus,infotbl($1a)); { subordinate bus }
                        end;
                      2: { CardBus }
                        begin

                          for i:=infotbl($19) to infotbl($1a) do
                            behind_secondary[i]:=i>infotbl($19);

                          if bustype[infotbl($19)] in [bustype_None,
                                                       bustype_PCI_known] then
                            bustype[infotbl($19)]:=bustype_CardBus;
                          pci_hibus:=Max(pci_hibus,infotbl($19)); { secondary bus   }
                          pci_hibus:=Max(pci_hibus,infotbl($1a)); { subordinate bus }
                        end;
                    end;

                  if showtree and (tree_used<High(tree)) then
                    begin
                      inc(tree_used);
                      with tree[tree_used] do
                        begin
                          b:=bus;
                          d:=deviceid;
                          f:=func;
                          vid:=infotbl_W($00);
                          did:=infotbl_W($02);
                        end;
                    end;

                  if terminate_request then
                    begin
                      close_pci_access_driver;
                      NormVideo;
                      Halt(9);
                    end;
                  showallinfo;
                end; { exist }

{ If not multi-device device, then don't test for func 1-7 as some cards
Incorrectly answer back on all 8 function numbers!!! S3 trio64, for example - stupid!  }
              if not is_multifunction then Break;

            end; { Function }

          { DeviceID }

        if (not gotone) and showtree and (tree_used<High(tree)) then
          begin
            Inc(tree_used);
            with tree[tree_used] do
              begin
                b:=bus;
                d:=0;
                f:=0;
                vid:=$ffff;
                did:=$ffff;
              end;
            Inc(tree_used_empty);
          end;
      end; { Bus }



{
  The following is an experiment with "Get IRQ Routing Info" BIOS function:
  the avid coder is free to un-comment the code and try it out: I couldn't
  make much sense out of the information returned myself!
}



  if dopcirouting then
    begin
      WriteLn;

      if dohtml then
        writeln_extra_html('<a name="pcirouting"></a>');

      WriteLn('PCI slot IRQ mapping information');

      failed:=true;
      FillChar(irqbuff,SizeOf(irqbuff),$00);
      load_irqbuff;

      if not failed then
        begin

          WriteLnColor(' PCI slot mapping information read successfully',LightGreen);
          {$IfDef Linux_OS2_Win32}
          if map_of_IRQ_channels_permanently_dedicated_to_PCI_unknown then
            WriteLnColor(' But IRQ permanently dedicated to PCI is unknown, assume none.',LightRed);
          {$EndIf Linux_OS2_Win32}
          WriteLn;


{ hex-dump table }
          if dumpregs then dohexdump;

{}
          WriteLn(' PCI slot IRQ availability listing');
          WriteLn;
          for i:=0 to (len shr 4)-1 do
            with irqbuffR.IRQ_routing_table_entry_Array[i] do
              begin
                WriteLn('  PCI Bus ',PCI_bus_number,', Device ',PCI_device_number shr 3,', Slot ',wrhex(device_slot_number));
                for j:=Low(INTABCD) to High(INTABCD) do
                  listmap(INTABCD[j].IRQ_connectivity_bit_map,'   INT'+Chr(Ord('A')+j)+'# can be connected to IRQs ');
                WriteLn;
              end;
          verify_routing_table(irqbuffR.IRQ_routing_table_entry_Array,len shr 4);
          WriteLn;


{}
          if dohtml then
            writeln_extra_html('<a name="slot_int_map"></a>');

          for i:=Low(link_use) to High(link_use) do
            link_use[i]:=[];

          FillChar(irq_usage_documented,SizeOf(irq_usage_documented),false);

          WriteLn(' PCI slot INTx to IRQ-router mappings');
          WriteLn;
          WriteLn('  SLOT BUS DEV  INTA INTB INTC INTD');
          for i:=0 to (len shr 4)-1 do
            with irqbuffR.IRQ_routing_table_entry_Array[i] do
              begin
                if PCI_bus_number=0 then
                  irq_usage_documented[PCI_device_number shr 3]:=true;
                Write('   ');
                if device_slot_number=0 then
                  Write('MB')
                else
                  Write(wrhex(device_slot_number));
                Write('  ',PCI_bus_number:2,'  ',PCI_device_number shr 3:2);
                Write('   ');

                for j:=Low(INTABCD) to High(INTABCD) do
                  Write(' ',wrhex(INTABCD[j].link_value),'  ');

                pins_used:=false;
                device_detected:=false;

                link_use_4[1]:=@link_use[INTABCD[0].link_value];
                link_use_4[2]:=@link_use[INTABCD[1].link_value];
                link_use_4[3]:=@link_use[INTABCD[2].link_value];
                link_use_4[4]:=@link_use[INTABCD[3].link_value];
                column_use_4[1]:=1;
                column_use_4[2]:=2;
                column_use_4[3]:=3;
                column_use_4[4]:=4;

                show_irqpin_use(PCI_bus_number,255,
                                PCI_device_number shr 3,PCI_device_number shr 3,
                                link_use_4,
                                column_use_4,
                                device_detected,pins_used);
                if not pins_used then
                  if device_detected then
                    WriteLn('No IRQ pin used')
                  else
                    WriteLn('No Device Detected');
              end;

          WriteLn;

          { check for bad tables: one link value can cause only one IRQ line }
          for i:=Low(link_use) to High(link_use) do
            begin
              count:=0;
              for h:=1 to 254 do
                if h in link_use[i] then
                  Inc(count);
              if count>1 then
                begin
                  TextColor(LightRed);
                  WriteLn('Error for link value ',wrhex(i),':');
                  Write  ('can not connect to more than one of IRQ ');
                  count:=0;
                  for h:=1 to 254 do
                    if h in link_use[i] then
                      begin
                        if count>0 then Write(',');
                        Write(h);
                        Inc(count);
                      end;
                  WriteLn;
                  TextColor(LightGray);
                end;
            end;


          { check for bad tables: not listed devices for bus 0}
          for i:=Low(irq_usage_documented) to High(irq_usage_documented) do
            if not irq_usage_documented[i] then
              begin
                { only look on first function }
                set_lookup_device(i,0,0);
                if infotbl_W(0)<>$ffff then
                  if infotbl($3c) in [1..254] then
                    if infotbl($3d) in [1..4] then
                      begin
                        TextColor(LightRed);
                        Write('Table error: Bus=0, Device=',i,' not listed! ');
                        cmpstr:=wrhexw(infotbl_W(0));
                        lookupven(true);
                        cmpstr:=wrhexw(infotbl_W(2));
                        lookupdev(numbers_of_screen_columns-45,true);
                        TextColor(LightGray);
                        WriteLn;
                      end;
              end;

{}
          listmap(conmap,' IRQ''s dedicated to PCI : ');

        end
      else
        begin
          WriteLnColor(' Unable to read slot mapping information from PCI BIOS!',LightRed);
        end;
      WriteLn;

    end; { dopcirouting }



{ BIOS IRQ Routing table tests }
  if dorouting then
    showroutinginfo;

{ summarial IRQ info }
  if not installermode then
    do_irqsummary;

{ bus/dev/func tree }
  if showtree and not installermode then
    do_showtree;

{ all done, so shutdown and quit }
  close_pci_access_driver;
  NormVideo;
end.

