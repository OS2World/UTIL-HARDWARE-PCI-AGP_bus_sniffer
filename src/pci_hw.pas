(* access of pci hardware *)

{$IfDef OS2}
  {$Define Linux_OS2_Win32}
{$EndIf}

{$IfDef Win32}
  {$Define Linux_OS2_Win32}
{$EndIf}

{$IfDef Linux}
  {$Define Linux_OS2_Win32}
{$EndIf}

{&Use32+}
{$G+}
{$R+}
{$S-}
{$I+}
{$N-}

(* to test IRQ routing table search code [Linux_OS2_Win32 only] *)
{ --$Define Debug_ROM}

unit pci_hw;

interface

type
  {$IfNDef VirtualPascal}
  SmallWord     =Word;
  {$EndIf}
  PSmallWord    =^SmallWord;
  PLongint      =^Longint;

  TIRQ_routing_table_entry_Array                =array[0..63] of
        packed record
          case integer of
            1:(binary                           :array[0..15] of byte);
            2:(PCI_bus_number                   :byte;
               PCI_device_number                :byte;
               INTABCD                          :array[0..3] of
                 packed record
                   link_value                   :byte;
                   IRQ_connectivity_bit_map     :SmallWord;
                 end;
               device_slot_number               :byte;
               reserved                         :byte);
        end;
  PIRQ_routing_table_entry_Array                =^TIRQ_routing_table_entry_Array;


{$IfDef Linux_OS2_Win32}
const
  map_of_IRQ_channels_permanently_dedicated_to_PCI_unknown:boolean=false;
{$EndIf Linux_OS2_Win32}

const
  usebios       :boolean=true;

var
  errcode       :byte;
  failed        :boolean;

  PCIverhi,
  PCIverlo,
  PCIchar,
  PCI_hibus     :byte;

  conmap,
  len           :word;

  irqbuff       : array[0..1023+6] of byte;
  irqbuffR      :
    packed record
      length_of_IRQ_routing_table_buffer        :SmallWord;
      IRQ_routing_table_array_buffer            :pointer;
      IRQ_routing_table_entry_Array             :TIRQ_routing_table_entry_Array;
    end absolute irqbuff;

procedure pci_present_test;
procedure search_pci_device(const vendor,device,search:word;var found:word);
function  lookup      (deviceid,func,bus:byte;index_:word) : byte;
function  lookup_dword(deviceid,func,bus:byte;index_:word) : longint;
procedure write_dword (deviceid,func,bus:byte;index_:word;data:longint);
procedure write_word  (deviceid,func,bus:byte;index_:word;data:smallword);
procedure write_byte  (deviceid,func,bus:byte;index_:word;data:byte);

function Mem_F000 (const i:word):byte;
function MemW_F000(const i:word):smallword;
function MemL_F000(const i:word):longint;
function Ptr_F000 (const i:word):pointer;
procedure load_irqbuff;

procedure open_pci_access_driver;
procedure close_pci_access_driver;
{$IfDef Linux_OS2_Win32}
procedure set_bios(const bios);
{$EndIf}

implementation

{$IfDef DPMI32}
uses
  Dpmi32Df,
  Dpmi32;
{$EndIf}

{$IfDef OS2}
uses
  Os2Base,
  Os2Def,
  Strings;
{$EndIf}


{$IfDef Win32}
uses
  VpSysLow,
  Strings,
  gwiopm;
{$EndIf}

{$IfDef Linux}
uses
  Dos,
  Strings;
{$EndIf}

const
  pci_access_driver_open:boolean=false;

type
  {$IfDef VirtualPascal}
  biosf000_type         =array[0..$ffff] of byte;
  {$Else}
  biosf000_type         =array[0..$fffe] of byte;
  {$EndIf}

const
  biosf000              :^biosf000_type=nil;

{$IfDef Linux_OS2_Win32}
procedure set_bios(const bios);
  begin
    if not Assigned(biosf000) then
      New(biosf000);
    Move(bios,biosf000^,SizeOf(biosf000^));
  end;
{$EndIf}

{$IfDef OS2}
procedure os2_read_bios;

  var
    hand,
    action,
    rc                  :longint;

    ParmRec1:
      record            // Input parameter record
        phys32          :longint;
        laenge          :smallword;
      end;

    ParmRec2:
      record
        sel             :smallword;
      end;

    ParmLen             : ULong;  // Parameter length in bytes
    DataLen             : ULong;  // Data length in bytes
    Data1:
      record
        sel             :smallword;
      end;

  begin
    if not Assigned(biosf000) then
      New(biosf000);
    FillChar(biosf000^,SizeOf(biosf000^),0);

    if DosOpen('SCREEN$',hand,action,0,0,1,$40,nil)<>0 then
      Exit;

    ParmLen:=SizeOf(ParmRec1);

    with ParmRec1 do
      begin
        phys32:=$000f0000;
        laenge:=0;
      end;

    datalen:=SizeOf(data1);
    rc:=DosDevIOCtl(
            hand,                       // Handle to device
            IOCTL_SCR_AND_PTRDRAW,      // Category of request
            SCR_ALLOCLDT,               // Function being requested
            @ParmRec1,                  // Input/Output parameter list
            ParmLen,                    // Maximum output parameter size
            @ParmLen,                   // Input:  size of parameter list
                                        // Output: size of parameters returned
            @Data1,                     // Input/Output data area
            Datalen,                    // Maximum output data size
            @DataLen);                  // Input:  size of input data area
    if rc=0 then
      begin

        asm {&Saves None}
          push gs

            sub esi,esi
            mov gs,data1.sel

            mov edi,[biosf000]
            mov ecx,$10000
            cld
          @l1:
            mov al,gs:[esi]
            inc esi
            stosb
            loop @l1

          pop gs
        end;

        ParmLen:=SizeOf(ParmRec2);

        with ParmRec2 do
          begin
            sel:=data1.sel;
          end;

        DataLen:=0;
        rc:=DosDevIOCtl(
                hand,                           // Handle to device
                IOCTL_SCR_AND_PTRDRAW,          // Category of request
                SCR_DEALLOCLDT,                 // Function being requested
                @ParmRec2,                      // Input/Output parameter list
                ParmLen,                        // Maximum output parameter size
                @ParmLen,                       // Input:  size of parameter list
                                                // Output: size of parameters returned
                nil,                            // Input/Output data area
                Datalen,                        // Maximum output data size
                @DataLen);                      // Input:  size of input data area

      end;

    DosClose(hand);
  end;

{$EndIf OS2}



function Mem_F000(const i:word):byte;
  begin
    Mem_F000:=biosf000^[i];
  end;

function MemW_F000(const i:word):SmallWord;
  begin
    MemW_F000:=PSmallWord(@biosf000^[i])^;
  end;

function MemL_F000(const i:word):longint;
  begin
    MemL_F000:=PLongint(@biosf000^[i])^;
  end;

function Ptr_F000(const i:word):pointer;
  begin
    Ptr_F000:=@biosf000^[i];
  end;


{$IfDef OS2}
var
  oemhlp_handle :longint=-1;

procedure open_oemhlp;
  begin
    if SysFileOpen('OEMHLP$',open_access_readonly+open_share_denynone,oemhlp_handle)<>0 then
      begin
         oemhlp_handle:=-1;
         failed:=true;
      end
    else
      failed:=false;
  end;

procedure close_oemhlp;
  begin
    SysFileClose(oemhlp_handle);
  end;
{$EndIf OS2}

{$IfDef VirtualPascal}

{$IfDef OS2}
function lookup_bios(deviceid,func,bus:byte;index_:word) : byte;

  var
    para              :
      packed record
        subfuction    :byte;
        busnumber     :byte;
        devfuncnumber :byte;
        configregister:byte;
        size          :byte;
      end;

    data              :
      packed record
        returncode    :byte;
        data          :longint;
      end;

    para_len,data_len :longint;

    rc                :longint;

  begin
    with para do
      begin
        subfuction:=3; (* read configuartion byte ($1a/$b108) *)
        busnumber:=bus;
        devfuncnumber:=deviceid shl 3+func;
        configregister:=index_;
        size:=SizeOf(byte);
      end;
    para_len:=SizeOf(para);

    with data do
      begin
        returncode:=0;
        data:=0;
      end;
    data_len:=SizeOf(data);

    rc:=
      DosDevIoCtl(
        oemhlp_handle,
        $80,              (* oemhlp/testcfg/.. *)
        $0b,              (* PCI *)
        @para,SizeOf(para),@para_len,
        @data,SizeOf(data),@data_len);

    errcode:=data.returncode;

    if (rc=0) and (errcode=$00) then
      begin
        failed:=false;
        lookup_bios:=Lo(data.data);
      end;
  end;

function lookup_dword_bios(deviceid,func,bus:byte;index_:word) : longint;

  var
    para              :
      packed record
        subfuction    :byte;
        busnumber     :byte;
        devfuncnumber :byte;
        configregister:byte;
        size          :byte;
      end;

    data              :
      packed record
        returncode    :byte;
        data          :longint;
      end;

    para_len,data_len :longint;

    rc                :longint;

  begin
    with para do
      begin
        subfuction:=3; (* read configuartion byte ($1a/$b10a) *)
        busnumber:=bus;
        devfuncnumber:=deviceid shl 3+func;
        configregister:=index_;
        size:=SizeOf(longint);
      end;
    para_len:=SizeOf(para);

    with data do
      begin
        returncode:=0;
        data:=0;
      end;
    data_len:=SizeOf(data);

    rc:=
      DosDevIoCtl(
        oemhlp_handle,
        $80,              (* oemhlp/testcfg/.. *)
        $0b,              (* PCI *)
        @para,SizeOf(para),@para_len,
        @data,SizeOf(data),@data_len);

    errcode:=data.returncode;

    if (rc=0) and (errcode=$00) then
      begin
        failed:=false;
        lookup_dword_bios:=data.data;
      end;
  end;


procedure write_data_bios(deviceid,func,bus:byte;index_:word;value:longint;datasize:byte);
  var
    para              :
      packed record
        subfuction    :byte;
        busnumber     :byte;
        devfuncnumber :byte;
        configregister:byte;
        size          :byte;
        data          :longint;
      end;

    data              :
      packed record
        returncode    :byte;
      end;

    para_len,data_len :longint;

    rc                :longint;

  begin
    with para do
      begin
        subfuction:=4; (* write configuartion space *)
        busnumber:=bus;
        devfuncnumber:=deviceid shl 3+func;
        configregister:=index_;
        size:=datasize;
        data:=value;
      end;
    para_len:=SizeOf(para);

    with data do
      begin
        returncode:=0;
      end;
    data_len:=SizeOf(data);

    rc:=
      DosDevIoCtl(
        oemhlp_handle,
        $80,              (* oemhlp/testcfg/.. *)
        $0b,              (* PCI *)
        @para,SizeOf(para),@para_len,
        @data,SizeOf(data),@data_len);

    errcode:=data.returncode;

    if (rc=0) and (errcode=$00) then
      failed:=false;
  end;

procedure pci_present_test;
  var
    para              :
      packed record
        subfuction    :byte;
      end;

    data              :
      packed record
        returncode    :byte;
        hardwaremech  :byte;
        majorver      :byte;
        minorver      :byte;
        lastbus       :byte;
      end;

    para_len,data_len :longint;

    rc                :longint;

  begin
    with para do
      begin
        subfuction:=0; (* Query PCI *)
      end;
    para_len:=SizeOf(para);

    FillChar(data,SizeOf(data),0);
    data_len:=SizeOf(data);

    rc:=
      DosDevIoCtl(
        oemhlp_handle,
        $80,              (* oemhlp/testcfg/.. *)
        $0b,              (* PCI *)
        @para,SizeOf(para),@para_len,
        @data,SizeOf(data),@data_len);

    errcode:=data.returncode;

    if (rc=0) and (errcode=$00) then
      with data do
        begin
          PCIchar:=hardwaremech;
          PCI_hibus:=lastbus;
          PCIverlo:=minorver;
          PCIverhi:=majorver;
          failed:=false;
        end;
  end;

procedure search_pci_device(const vendor,device,search:word;var found:word);
  var
    para              :
      packed record
        subfuction    :byte;
        device_id     :smallword;
        vendor_id     :smallword;
        index_        :byte;
      end;

    data              :
      packed record
        returncode    :byte;
        busnumber     :byte;
        devfuncnumber :byte;
      end;

    para_len,data_len :longint;

    rc                :longint;

  begin
    with para do
      begin
        subfuction:=1; (* Find PCI device *)
        device_id:=device;
        vendor_id:=vendor;
        index_:=search;
      end;
    para_len:=SizeOf(para);

    FillChar(data,SizeOf(data),0);
    data_len:=SizeOf(data);

    rc:=
      DosDevIoCtl(
        oemhlp_handle,
        $80,              (* oemhlp/testcfg/.. *)
        $0b,              (* PCI *)
        @para,SizeOf(para),@para_len,
        @data,SizeOf(data),@data_len);

    errcode:=data.returncode;

    if (rc=0) and (errcode=$00) then
      with data do
        begin
          found:=busnumber shl 8+devfuncnumber;
          failed:=false;
        end
    else
      failed:=true;

  end;

{$EndIf OS2}

{$IfDef Linux_OS2_Win32}

procedure load_irqbuff;

  function compare(const bios;const code:string):boolean;
    var
      z               :longint;
      bios_a          :array[1..100] of char absolute bios;
    begin
      compare:=false;

      for z:=1 to Length(code) do
        if  (bios_a[z]<>code[z]) and (code[z]<>'?') then
          Exit
        else
          if z=Length(code) then
            begin
              compare:=true;
              Exit;
            end;
    end;

  const
    award_call        :string
      =#$3c#$0e                         // cmp al,$0e
      +#$75#$05                         // jne @al_0f
      +#$e8'??'                         // call pci_0e
      +#$eb'?'                          // jmp @exit
  (*  +#$3c#$0f this code is not present on K8NSC939.BIN(F6)
      +#$75#$05
      +#$e8'??'*);

    award_pci_0e      :string
      =#$be'??'                         // mov si,0B90
      +#$8b#$0c                         // mov cx,[si]
      +#$26#$3b#$0d                     // cmp cx,es:[di]
      +#$26#$89#$0d                     // mov es:[di],cx
      +#$76#$06                         // jbe @0F9D6B
      +#$C6#$46#$01#$89                 // mov byte ptr [bp+1],89
      +#$F9                             // stc
      +#$C3                             // ret
                                        // @0F9D6B:
      +#$06                             // push es
      +#$26#$C4#$7D#$02                 // les di,es:[di+2]
      +#$BE'??'                         // mov si,0B20
      +#$C1#$E9#$02                     // shr cx,2
      +#$FC                             // cld
      +#$F3#$66#$A5                     // repz movsd
      +#$07                             // pop es
      +#$c6#$46#$01#$00                 // mov byte ptr [bp+1],0
      +#$f8                             // clc
      +#$c3;                            // ret

    award_pci_0e_2    :string
      =#$60                             // pusha
      +#$be'??'                         // mov si,0B90
      +#$2e#$8b#$0c                     // mov cx,cs:[si]
      +#$26#$3b#$0d                     // cmp cx,es:[di]
      +#$26#$89#$0d                     // mov es:[di],cx
      +#$76#$05                         // jbe @0FAECC
      +#$61                             // popa
      +#$b4#$89                         // mov ah,89
      +#$F9                             // stc
      +#$C3                             // ret
                                        // @0FAECC:
      +#$06                             // push es
      +#$26#$C4#$7D#$02                 // les di,es:[di+2]
      +#$BE'??'                         // mov si,DC50
      +#$FC                             // cld
      +#$F3#$A4                         // repz movsb
      +#$07                             // pop es
      +#$61                             // popa
      +#$e8'??'                         // call b510 - Get_PCI_IRQ_Record
      +#$b4#$00                         // mov ah,00
      +#$f8                             // clc
      +#$c3;                            // ret

    award_pci_0e_3    :string (* sis496 *)
      =#$66#$60                         // pushad
      +#$be'??'                         // mov si,0B90
      +#$8b#$0c                         // mov cx,[si]
      +#$26#$3b#$0d                     // cmp cx,es:[di]
      +#$26#$89#$0d                     // mov es:[di],cx
      +#$76#$06                         // jbe @0FBC65
      +#$66#$61                         // popad
      +#$b4#$89                         // mov ah,89
      +#$F9                             // stc
      +#$C3                             // ret
                                        // @0FBC65:
      +#$06                             // push es
      +#$26#$C4#$7D#$02                 // les di,es:[di+2]
      +#$BE'??'                         // mov si,DC50
      +#$C1#$E9#$02                     // shr cx,2
      +#$FC                             // cld
      +#$F3#$66#$a5                     // repz movsd
      +#$07                             // pop es
      +#$66#$61                         // popad
      +#$b4#$00                         // mov ah,00
      +#$f8                             // clc
      +#$c3;                            // ret



    award_Get_PCI_IRQ_Record :string (* pci32.asm *)
      =#$b3'?'                          // mov bl,_lo
      +#$b7'?'                          // mov bh,_hi
      +#$c3;

    systemsoft_pci_0e :string
      =#$66#$57                         // push edi
      +#$F6#$C4#$40                     // test ah, 40h
      +#$75#$07                         // jnz @792E
                                        //
      +#$66#$81#$E7#$FF#$FF#$00#$00     // and edi,$FFFF
                                        //
      +#$67#$26#$81#$3F'??'             // cmp word ptr es:[edi],$80h
      +#$72'?'                          // jb zu_klein
                                        //
      +#$06                             // push es
      +#$66#$57                         // push edi
      +#$67#$26#$C7#$07#$00#$00         // mov word ptr es:[edi],0
      +#$F6#$C4#$40                     // test ah, 40h
      +#$74#$15                         // jz @7959
                                        //
      +#$67#$66#$26#$C4#$7F#$02         // les  edi, es:[edi+2]
      +#$66#$BE#$00#$80#$0E#$00         // mov esi,$E8000
      +#$66#$81#$C6'??'#$00#$00         // add esi,offset PCI_routing_tabelle
      +#$EB#$0B                         // jmp short @7964
                                        //
                                        // @7959:
      +#$67#$26#$C4#$7F#$02             // les di,es:[edi+2]
      +#$66#$BE'??'#$00#$00             // mov esi,offset PCI_routing_tabelle
                                        //
                                        // @7964:
      +#$66#$B9'??'#$00#$00;            // mov ecx,$80

    ami_pci_0e :string
      =#$66#$51                         // 00 push  ecx
      +#$50                             // 02 push  ax
      +#$66#$56                         // 03 push  esi
      +#$66#$57                         // 05 push  edi
      +#$06                             // 07 push  es
      +#$66#$0F#$B7#$FF                 // 08 movzx edi, di
      +#$2E#$0F#$B6#$0E'??'             // 0c movzx cx, cs:anzahl_eintraege
      +#$2E#$8B#$1E'??'                 // 12 mov   bx, cs:irq_bitmap
      +#$BE'??'                         // 17 mov   si, offset irq_rout_tab
      +#$C1#$E1#$04                     // 1a shl   cx, 4
      +#$67#$26#$8B#$07                 // 1d mov   ax, es:[edi]
      +#$67#$26#$89#$0F                 // 21 mov   es:[edi], cx
      +#$3B#$C1                         // 25 cmp   ax, cx
      +#$72#$15                         // 27 jb    loc_0_8159
      +#$67#$26#$C4#$7F#$02             // 29 les   di, es:[edi+2]
      +#$F3#$A4                         // 2e repe movsb
      +#$32#$ED                         // 30 xor   ch, ch
      +#$F8;                            // 32 clc

    (* D.E.: "AMIBIOS 063200  03/01/99(C)1998 American Megatrends Inc."
             "51-2300-000000-00101111-030199-" *)

    ami2_pci_0e :string
      =#$66#$51                         // 00 push  ecx
      +#$66#$50                         // 02 push  eax
      +#$66#$56                         // 04 push  esi
      +#$66#$57                         // 06 push  edi
      +#$06                             // 08 push  es
      +#$66#$0f#$b7#$ff                 // 09 movzx edi, di
      +#$2e#$0f#$b6#$0e'??'             // 0d movzx cx, cs:anzahl_eintraege
      +#$2e#$8b#$1e'??'                 // 13 mov   bx, cs:irq_bitmap
      +#$be'??'                         // 18 mov   si, offset irq_rout_tab
      +#$c1#$e1#$04                     // 1b shl   cx, 4
      +#$67#$26#$8b#$07                 // 1e mov   ax, es:[edi]
      +#$67#$26#$89#$0f                 // 22 mov   es:[edi], cx
      +#$3b#$c1                         // 26 cmp   ax, cx
      +#$72#$16                         // 28 jb    loc_0_2e05
      +#$67#$26#$c4#$7f#$02             // 2a les   di, es:[edi+2]
      +#$f3#$a4                         // 2f repe movsb
      +#$32#$ed                         // 31 xor   ch, ch
      +#$f8;                            // 33 clc


    (* IBM Thinkpad 390E with Phoenix BIOS *)
    phoenix_pci_0e : string
      =#$06                             // push  es
      +#$FC                             // cld
      +#$26#$8B#$15                     // mov   dx, es:[di]
      +#$B8'??'                         // mov   ax, 80h
      +#$AB                             // stosw
      +#$3B#$D0                         // cmp   dx, ax
      +#$72#$1E                         // jb    loc_0_B121
      +#$67#$C7#$45#$10#$00#$00         // mov   word ptr [ebp+10h], 0
      +#$67#$8D#$35'??'#$00#$00         // lea   si, large ds:7C0h
      +#$E8'??'                         // call  fd7a0
      +#$26#$C4#$3D                     // les   di, es:[di]
      +#$66#$B9'??'#$00#$00             // mov   ecx, 80h
      +#$F3#$A4                         // repe movsb
      +#$F8                             // clc
      +#$EB#$06                         // jmp   short loc_0_B127
                                        // loc_0_B121:
      +#$67#$C6#$45#$1D#$89             // mov   byte ptr [ebp+1Dh], 89h
      +#$F9                             // stc
                                        // loc_0_B127:
      +#$07                             // pop   es
      +#$C3;                            // retn

    phoenix_pci_fd7a0 : String
      =#$50                             // push  ax
      +#$B8'??'                         // mov   ax, 0FD7Ah
      +#$C1#$E0#$04                     // shl   ax, 4
      +#$03#$F0                         // add   si, ax
      +#$58                             // pop   ax
      +#$C3;                            // retn



    pir_signature       =Ord('$')+Ord('P') shl 8+Ord('I') shl 16+Ord('R') shl 24;

  type
    pci_irq_table       =
      packed record
        signature       :longint;
        version         :smallword;
        size            :smallword;
        router_bus_devno:smallword;
        pci_exclusive   :smallword;
        irq_rout_vendor :smallword;
        irq_rout_device :smallword;
        miniport        :longint;
        reserved        :array[$14..$1e] of byte;
        checksum        :byte;
        slottable       :array[0..0] of array[0..15] of byte;
      end;


  var
    ca,pci_0e,z         :longint;
    irqmap_code         :longint;

    i,j                 :longint;
    sum                 :byte;

  begin
    (* failed:=true; *)

    if (StrLComp(@biosf000^[$e000],'Award',Length('Award'))=0)
    or (StrLComp(@biosf000^[$e000],'Phoen',Length('Phoen'))=0)
     then
      begin (* Award BIOS *)
        ca:=0;
        repeat

          if biosf000^[ca]=Ord(award_call[1]) then
            if compare(biosf000^[ca],award_call) then
              begin
                pci_0e:=ca+4+3+biosf000^[ca+5]+biosf000^[ca+6] shl 8;
                if compare(biosf000^[pci_0e],award_pci_0e) then
                  begin
                    conmap:=0;
                    len:=PSmallWord(@biosf000^[PSmallWord(@biosf000^[pci_0e+1])^])^;
                    irqbuffR.length_of_IRQ_routing_table_buffer:=len;
                    Move(biosf000^[PSmallWord(@biosf000^[pci_0e+25])^],irqbuffR.IRQ_routing_table_entry_Array,len);
                    failed:=false;
                    Exit;
                  end

                else
                if compare(biosf000^[pci_0e],award_pci_0e_2) then
                  begin
                    conmap:=0;
                    len:=PSmallWord(@biosf000^[PSmallWord(@biosf000^[pci_0e+2])^])^;
                    irqbuffR.length_of_IRQ_routing_table_buffer:=len;
                    Move(biosf000^[PSmallWord(@biosf000^[pci_0e+26])^],irqbuffR.IRQ_routing_table_entry_Array,len);
                    failed:=false;

                    (* try to find IRQ map *)
                    irqmap_code:=(pci_0e+33+PSmallWord(@biosf000^[pci_0e+34])^+3) and $ffff;
                    if compare(biosf000^[irqmap_code],award_Get_PCI_IRQ_Record) then
                      conmap:=biosf000^[irqmap_code+1]
                             +biosf000^[irqmap_code+3] shl 8
                    else
                      map_of_IRQ_channels_permanently_dedicated_to_PCI_unknown:=true;
                    Exit;
                  end
                else
                if compare(biosf000^[pci_0e],award_pci_0e_3) then
                  begin
                    conmap:=0;
                    len:=PSmallWord(@biosf000^[PSmallWord(@biosf000^[pci_0e+3])^])^;
                    irqbuffR.length_of_IRQ_routing_table_buffer:=len;
                    Move(biosf000^[PSmallWord(@biosf000^[pci_0e+27])^],irqbuffR.IRQ_routing_table_entry_Array,len);
                    failed:=false;
                  end

              end;

          Inc(ca);
        until ca>$fff0;

      end; (* Award BIOS *)


    if StrLComp(@biosf000^[$ff60],'AMI,',Length('AMI,'))=0 then
      (* '(C)1996AMI,770-246-8600', kein $PIR, nur privates $IRT *)
      begin
        pci_0e:=0;
        repeat
          if biosf000^[pci_0e]=Ord(ami_pci_0e[1]) then
            if compare(biosf000^[pci_0e],ami_pci_0e) then
              begin
                conmap:=0;
                len:=biosf000^[PSmallWord(@biosf000^[pci_0e+$0c+4])^] shl 4;
                irqbuffR.length_of_IRQ_routing_table_buffer:=len;
                conmap:=PSmallWord(@biosf000^[PSmallWord(@biosf000^[pci_0e+$12+3])^])^;
                Move(biosf000^[PSmallWord(@biosf000^[pci_0e+$17+1])^],irqbuffR.IRQ_routing_table_entry_Array,len);
                failed:=false;
                Exit;
              end;

          if biosf000^[pci_0e]=Ord(ami2_pci_0e[1]) then
            if compare(biosf000^[pci_0e],ami2_pci_0e) then
              begin
                conmap:=0;
                len:=biosf000^[PSmallWord(@biosf000^[pci_0e+$0d+4])^] shl 4;
                irqbuffR.length_of_IRQ_routing_table_buffer:=len;
                conmap:=PSmallWord(@biosf000^[PSmallWord(@biosf000^[pci_0e+$13+3])^])^;
                Move(biosf000^[PSmallWord(@biosf000^[pci_0e+$18+1])^],irqbuffR.IRQ_routing_table_entry_Array,len);
                failed:=false;
                Exit;
              end;


          Inc(pci_0e);

        until pci_0e>$ff00;
      end; (* AMI $IRT *)

    if StrLComp(@biosf000^[$e010],'SystemSoft',Length('SystemSoft'))=0 then
      begin
        pci_0e:=0;
        repeat
          if biosf000^[pci_0e]=Ord(systemsoft_pci_0e[1]) then
            if compare(biosf000^[pci_0e],systemsoft_pci_0e) then
              begin
                conmap:=0;
                len:=PSmallWord(@biosf000^[pci_0e+14+4])^;
                irqbuffR.length_of_IRQ_routing_table_buffer:=len;
                Move(biosf000^[PSmallWord(@biosf000^[pci_0e+48+3])^],irqbuffR.IRQ_routing_table_entry_Array,len);
                failed:=false;
                Exit;
              end;

          Inc(pci_0e);

        until pci_0e>$ff00;
      end; (* SystemSoft *)

    (* ther is no good signature on the Thinkpad 390E *)
    if true then
      begin
        pci_0e:=0;
        repeat
          if biosf000^[pci_0e]=Ord(phoenix_pci_0e[1]) then
            if compare(biosf000^[pci_0e],phoenix_pci_0e) then
              begin
                (* follow call to data part base procedure *)
                i:=(pci_0e+26+3+MemW_F000(pci_0e+26+1)) and $ffff;
                if compare(biosf000^[i],phoenix_pci_fd7a0) then
                  begin
                    conmap:=0;
                    len:=MemW_F000(pci_0e+5+1);
                    irqbuffR.length_of_IRQ_routing_table_buffer:=len;
                    Move(biosf000^[  MemW_F000(pci_0e+19+3)
                                   +(MemW_F000(i     +1 +1)-$f000) shl 4],
                         irqbuffR.IRQ_routing_table_entry_Array,
                         len);
                    failed:=false;
                    Exit;
                  end;
              end;

          Inc(pci_0e);

        until pci_0e>$ff00;
      end; (* Phoenix *)

    (* generic version, but can not return conmap value *)
    i:=$0000;

    repeat
      with pci_irq_table(biosf000^[i]) do

        (* search valid table *)
        if  (signature=pir_signature)
        and (size>$20)
        and (version>=$0100) then
          begin
            sum:=0;
            for j:=i to i+size-1 do
              Inc(sum,biosf000^[j]);

            if sum=0 then (* use the info *)
              with irqbuffR do
                begin
                  len:=size-$20;
                  length_of_IRQ_routing_table_buffer:=len;
                  conmap:=0; (* unknown ! *)
                  map_of_IRQ_channels_permanently_dedicated_to_PCI_unknown:=true;
                  if pci_exclusive<>0 then
                    begin
                      conmap:=pci_exclusive;
                      map_of_IRQ_channels_permanently_dedicated_to_PCI_unknown:=false;
                    end;

                  IRQ_routing_table_array_buffer:=Ptr($f0000000+i+$20); (* not used *)
                  Move(biosf000^[i+$20],IRQ_routing_table_entry_Array,length_of_IRQ_routing_table_buffer);

                  failed:=false;
                  Exit;
                end;
          end;

      Inc(i,$0010)
    until i>$ffff;


  end;

{$EndIf Linux_OS2_Win32}

{$IfDef DPMI32}
function lookup_bios(deviceid,func,bus:byte;index_:word) : byte;assembler;
  {&Frame-}{&Uses ebx,ecx,edx,edi}
  asm
    mov ax,$b108
    mov bl,deviceid
    shl bl,3
    or  bl,func
    mov bh,bus
    mov edi,index_
    int $1a
    jc  @exit

    mov failed,false
  @exit:
    mov errcode,ah
    mov al,cl
  end;

function lookup_dword_bios(deviceid,func,bus:byte;index_:word) : longint;assembler;
  {&Frame-}{&Uses ebx,ecx,edx,edi}
  asm
    mov ax,$b10a
    mov bl,deviceid
    shl bl,3
    or  bl,func
    mov bh,bus
    mov edi,index_
    int $1a
    jc  @exit

    mov failed,false
  @exit:
    mov errcode,ah
    mov eax,ecx
  end;

procedure write_data_bios(deviceid,func,bus:byte;index_:word;data:longint;datasize:byte);assembler;
  {&Frame-}{&Uses All}
  asm
    mov eax,$b10b               (* byte *)
    cmp datasize,1
    je @size_correct
    mov al,$0c                  (* word *)
    cmp datasize,2
    je @size_correct
    mov al,$0d                  (* dword *)
  @size_correct:
    mov bl,deviceid
    shl bl,3
    or  bl,func
    mov bh,bus
    mov edi,index_
    mov ecx,data
    int $1a
    mov errcode,ah
  end;


procedure pci_present_test;assembler;
  {&Frame-}{&Uses All}
  asm
    mov ax,$b101
    int $1a
    jc  @exit

{ check signature bytes OK }
    cmp dx,$4350
    jne @exit

{ check no error code returned > AH=00=Success }
    cmp ah,0
    jne @exit

    mov PCIchar,al
    mov PCI_hibus,cl
    mov PCIverlo,bl
    mov PCIverhi,bh
    mov failed,false

  @exit:
  end;

procedure search_pci_device(const vendor,device,search:word;var found:word);assembler;
  {&Frame-}{&Uses All}
  asm
    mov ax,$b102
    mov ecx,[device]
    mov edx,[vendor]
    mov esi,[search]
    int $1a
    setc failed
    mov edi,[found]
    and ebx,$ffff
    mov [edi],ebx
  end;


procedure load_irqbuff;
  var
    irq16     :smallword;
    r         :real_mode_call_structure_typ;
  begin
    if GetDosMem(irq16,SizeOf(irqbuff))<>0 then Exit;
    FillChar(Mem[irq16 shl 4],SizeOf(irqbuff),0);

    MemW[irq16 shl 4+0]:=SizeOf(irqbuff)-6;
    MemW[irq16 shl 4+2]:=6;
    MemW[irq16 shl 4+4]:=irq16;

    with r do
      begin
        init_register(r);
        ax_:=$b10e;
        bx_:=$0000;
        ds_:=$f000;
        es_:=irq16;
        edi_:=0;

        intr_realmode(r,$1a);
        Move(Ptr(irq16 shl 4)^,irqbuff,SizeOf(irqbuff));
        len:=MemW[es_ shl 4+edi_];
        freedosmem(irq16);

        if ah_<>0 then Exit;

        conmap:=bx_;
        failed:=false;

      end;
  end;
{$EndIf DPMI32}


function lookup_hw(deviceid,func,bus:byte;index_:word) : byte;assembler;
  {&Frame-}{&Uses ecx}
  asm
    mov ah,$80
    mov al,bus
    shl eax,16
    mov ecx,index_
    mov al,cl
    and al,$fc
    mov ah,deviceid
    shl ah,3
    or  ah,func

    push eax
    push $0cf8
    call _Out32

    and ecx,3
    shl ecx,3 (* *8 *)

    push $0cfc
    call _In32
    shr eax,cl
    mov ecx,eax
    mov failed,false

    push 0
    push $0cf8
    call _Out32

    mov eax,ecx // return al
  end;

function lookup_dword_hw(deviceid,func,bus:byte;index_:word) : longint; assembler;
  {&Frame-}{&Uses ecx}
  asm
    mov ah,$80
    mov al,bus
    shl eax,16
    mov ecx,index_
    mov al,cl
    test al,$03
    jnz @@ret
    mov ah,deviceid
    shl ah,3
    or  ah,func

    push eax
    push $0cf8
    call _Out32

    push $0cfc
    call _In32
    mov ecx,eax
    mov failed,false

    push 0
    push $0cf8
    call _Out32

    mov eax,ecx // return al
  @@ret:
  end;


procedure write_data_hw(deviceid,func,bus:byte;index_:word;data:longint;datasize:byte);assembler;
  {&Frame-}{&Uses eax,ecx}
  asm
    mov ah,$80
    mov al,bus
    shl eax,16
    mov ecx,index_
    mov al,cl
    and al,$fc
    mov ah,deviceid
    shl ah,3
    or  ah,func

    push eax
    push $0cf8
    call _Out32

    and ecx,3
    add ecx,$0cfc
    mov al,datasize

    push [data]
    push ecx
    cmp al,1
    jne @not_size_8
    call _Out8
    jmp @continue
  @not_size_8:
    cmp al,2
    jne @not_size_16
    call _Out16
    jmp @continue
  @not_size_16:
    call _Out32
  @continue:
    mov failed,false

    push 0
    push $0cf8
    call _Out32
  end;

{$IfDef Win32}

procedure initNT_system;
  const
    error_success       =0;
  var
    result              : longint;
  begin
    if SysPlatformID<2 then Exit;

    result:=gwiopm_driver.openscm;
    if result<>error_success then
      Writeln('NT: Open SCM Error : ',gwiopm_driver.errorlookup(result));

    result:=gwiopm_driver.install('');
    if result<>error_success then
      begin
        WriteLn('NT: Driver Problem detected: attempting to fix...');
        gwiopm_driver.stop;
        gwiopm_driver.remove;
        WriteLn('NT: Driver hopefully fixed, retrying...');
        result:=gwiopm_driver.install('');
      end;

    if result<>error_success then
      begin
        WriteLn('NT: Couldn''t Install/Start driver, error : ',gwiopm_driver.errorlookup(result),' : PCI_W halted!');
        WriteLn('NT: Ensure you have administrative rights, '
                   +'that GWIOPM.SYS is present, and that PCI_W is run from a local drive');
        Halt(12);
      end;

    gwiopm_driver.start;
    gwiopm_driver.closescm;
    gwiopm_driver.DeviceOpen;

    GWIOPM_Driver.LIOPM_Set_Ports($cf8, $cff, true);
    GWIOPM_Driver.IOCTL_IOPMD_ACTIVATE_KIOPM;
  end;



procedure killNT_system;
  begin
    if SysPlatformID<2 then Exit;

    GWIOPM_Driver.LIOPM_Set_Ports($cf8, $cff, false);
    GWIOPM_Driver.IOCTL_IOPMD_ACTIVATE_KIOPM;

    gwiopm_driver.DeviceClose;
    gwiopm_driver.openscm;
    gwiopm_driver.stop;
    gwiopm_driver.remove;
    gwiopm_driver.closescm;
  end;

procedure win32_read_bios;
  begin
    if not Assigned(biosf000) then
      New(biosf000);
    FillChar(biosf000^,SizeOf(biosf000^),0);

    failed:=true;
    // todo: implement copy BIOS to biosf000^ array variable!
  end;


procedure pci_present_test;
  begin
{ we would normally check for a PCI BIOS, and retrieve version & hi_bus number info here}


{ these values must at some point be gained from the 32-bit PCI BIOS; they are "faked" for now...!! }
{ must learn to call BIOS32 services properly.... }


    pciverhi:=2;                  { not valid - just a placeholder value }
    pciverlo:=$10;                { not valid - just a placeholder value }
    pci_hibus:=1;                 { just a guess ... }

    PCIchar:=0;
    failed:=false;

  end;

procedure search_pci_device(const vendor,device,search:word;var found:word);
  begin
    failed:=true;
  end;

function lookup_bios(deviceid,func,bus:byte;index_:word) : byte;
  begin
    lookup_bios:=lookup_hw(deviceid,func,bus,index_);
  end;

function lookup_dword_bios(deviceid,func,bus:byte;index_:word) : longint;
  begin
    lookup_dword_bios:=lookup_dword_hw(deviceid,func,bus,index_);
  end;

procedure write_data_bios(deviceid,func,bus:byte;index_:word;data:longint;datasize:byte);
  begin
    write_data_hw(deviceid,func,bus,index_,data,datasize);
  end;
{$EndIf Win32}

{$IfDef Linux}
procedure linux_read_bios;
  var
    f                   :file;
    rc                  :word;
  begin
    if not Assigned(biosf000) then
      New(biosf000);

    FillChar(biosf000^,SizeOf(biosf000^),0);

    Assign(f,'/dev/mem');
    FileMode:=$40;
    {$I-}
    Reset(f,1);
    {$I+}
    rc:=IOResult;
    if rc=0 then
      begin
        Seek(f,$f0000);
        BlockRead(f,biosf000^,SizeOf(biosf000^));
        Close(f);
      end
    else
      begin
        (* silently ignore..
        WriteLn('Can not read bios (/dev/mem), rc=',rc);
        Halt(rc); *)
      end;
  end;


procedure AssignBusDeviceFunc(var f:file;const bus,dev,func:byte);
  procedure Int2Hex(b,z:byte;t:pchar);
    const
      hexz:array[0..$f] of char='0123456789abcdef'; (* must be lower case *)
    begin
      while z>0 do
        begin
          Dec(z);
          t[0]:=hexz[b and $f];
          b:=b shr 4;
          Dec(t);
        end;
    end;
  var
    fn:string;
  begin
    fn:='/proc/bus/pci/??/??.?';
    Int2Hex(bus ,2,@fn[16]);
    Int2Hex(dev ,2,@fn[19]);
    Int2Hex(func,1,@fn[21]);
    Assign(f,fn);
  end;

function lookup_bios(deviceid,func,bus:byte;index_:word) : byte;
  var
    f                   :file;
    rc                  :word;
    temp                :array[0..3] of byte;
  begin
    AssignBusDeviceFunc(f,bus,deviceid,func);
    FileMode:=$40;
    {$I-}
    Reset(f,1);
    {$I+}
    rc:=IOResult;
    if rc=0 then
      begin
        {$I-}
        Seek(f,index_ and $fc);
        {$I+}
        if IOResult<>0 then
          begin
            Close(f);
            failed:=true;
            Result:=0;
            Exit;
          end;
        {$I-}
        BlockRead(f,temp,4);
        {$I+}
        if IOResult<>0 then
          begin
            Close(f);
            failed:=true;
            Result:=0;
            Exit;
          end;
        Close(f);
        failed:=false;
        Result:=temp[index_ and $03];
      end
    else
      begin
        failed:=true;
        Result:=$ff;
      end;
  end;

function lookup_dword_bios(deviceid,func,bus:byte;index_:word) : longint;
  var
    f                   :file;
    rc                  :word;
  begin
    if (index_ and $03)<>0 then
      begin
        failed:=true;
        result:=$ffffffff;
      end;

    AssignBusDeviceFunc(f,bus,deviceid,func);
    FileMode:=$40;
    {$I-}
    Reset(f,1);
    {$I+}
    rc:=IOResult;
    if rc=0 then
      begin
        {$I-}
        Seek(f,index_);
        {$I+}
        if IOResult<>0 then
          begin
            Close(f);
            failed:=true;
            Result:=0;
            Exit;
          end;
        {$I-}
        BlockRead(f,Result,4);
        {$I+}
        if IOResult<>0 then
          begin
            Close(f);
            failed:=true;
            Result:=0;
            Exit;
          end;
        Close(f);
        failed:=false;
      end
    else
      begin
        failed:=true;
        Result:=$ffffffff;
      end;
  end;

procedure write_data_bios(deviceid,func,bus:byte;index_:word;value:longint;datasize:byte);
  var
    f                   :file;
    rc                  :word;
    temp                :array[0..3] of byte;
  begin
    failed:=true;

    if (index_ and 3)+datasize>4 then
      Exit;

    if index_>$ff then Exit;

    AssignBusDeviceFunc(f,bus,deviceid,func);
    FileMode:=$42;
    {$I-}
    Reset(f,1);
    {$I+}
    if IOResult<>0 then
      Exit;

    if (index_ and 3<>0) or (datasize<>4) then
      begin
        {$I-}
        Seek(f,index_ and $fc);
        {$I+}
        if IOResult<>0 then
          begin
            Close(f);
            Exit;
          end;
        {$I-}
        BlockRead(f,temp,4);
        {$I+}
        if IOResult<>0 then
          begin
            Close(f);
            Exit;
          end;
      end;

    Move(value,temp[index_ and 3],datasize);
    {$I-}
    Seek(f,index_ and $fc);
    {$I+}
    if IOResult<>0 then
      begin
        Close(f);
        Exit;
      end;
    {$I-}
    BlockWrite(f,temp,4);
    {$I+}
    if IOResult<>0 then
      begin
        Close(f);
        Exit;
      end;

    Close(f);
    failed:=false;

  end;

procedure pci_present_test;
  var
    sr                  :SearchRec;
    pci_hibus_val       :byte;
    code                :integer;
  begin
{ we would normally check for a PCI BIOS, and retrieve version & hi_bus number info here}


{ these values must at some point be gained from the 32-bit PCI BIOS; they are "faked" for now...!! }
{ must learn to call BIOS32 services properly.... }


    pciverhi:=2;                  { not valid - just a placeholder value }
    pciverlo:=$10;                { not valid - just a placeholder value }
    pci_hibus:=0;                 { just a guess ... }

    FindFirst('/proc/bus/pci/*',AnyFile,sr);
    while Dos.DosError=0 do
      begin
        Val('$'+sr.name,pci_hibus_val,code);
        if code=0 then
          if pci_hibus<pci_hibus_val then
            pci_hibus:=pci_hibus_val;
        FindNext(sr);
      end;
    FindClose(sr);

    PCIchar:=0;
    failed:=false;

  end;

procedure search_pci_device(const vendor,device,search:word;var found:word);
  begin
    failed:=true;
  end;

{$EndIf Linux}

{$Else} (* BP 7.0 *)

function lookup_bios(deviceid,func,bus:byte;index_:word) : byte; assembler;
  asm
    mov ax,$b108
    mov bl,deviceid
    shl bl,3
    or  bl,func
    mov bh,bus
    mov di,index_
    int $1a
    jc @exit

    mov failed,false
    mov al,cl           (* result *)
  @exit:
    mov errcode,ah
  end;

function lookup_dword_bios(deviceid,func,bus:byte;index_:word) : longint; assembler;
  asm
    mov ax,$b10a
    mov bl,deviceid
    shl bl,3
    or  bl,func
    mov bh,bus
    mov di,index_
    int $1a
    jc @exit

    mov failed,false
    db $66
    push cx
    pop ax              (* result *)
    pop dx
  @exit:
    mov errcode,ah
  end;


function lookup_hw(deviceid,func,bus:byte;index:word) : byte; assembler;
  asm
    mov ah,$80
    mov al,bus
    db $66;shl ax,16

    mov ax,index
    and ax,$fc
    mov ah,deviceid
    shl ah,3
    or  ah,func

    mov dx,$cf8
    db $66;out dx,ax

    mov ax,index
    and ax,3
    shl ax,3
    mov cx,ax

    mov dx,$cfc
    db $66;in ax,dx
    db $66;shr ax,cl
    mov cx,ax

    mov failed,false

    db $66;xor ax,ax
    mov dx,$cf8
    db $66;out dx,ax

    mov ax,cx
  end;

function lookup_dword_hw(deviceid,func,bus:byte;index:word) : longint; assembler;
  asm
    mov ah,$80
    mov al,bus
    db $66;shl ax,16

    mov ax,index
    mov ah,deviceid
    shl ah,3
    or  ah,func

    mov dx,$cf8
    db $66;out dx,ax

    mov dx,$cfc
    db $66;in ax,dx
    db $66;push cx
    pop ax
    pop dx

    mov failed,false

    db $66;xor ax,ax
    mov dx,$cf8
    db $66;out dx,ax

    mov ax,cx
  end;


procedure write_data_bios(deviceid,func,bus:byte;index:word;data:longint;datasize:byte); assembler;
  asm
    mov ax,$b10b                (* byte *)
    cmp datasize,1
    je  @size_correct
    mov al,$0c                  (* word *)
    cmp datasize,2
    je  @size_correct
    mov al,$0d                  (* dword *)
  @size_correct:
    mov bl,deviceid
    shl bl,3
    or  bl,func
    mov bh,bus
    mov di,index
    db $66; mov cx,word ptr data
    int $1a
    mov errcode,ah
  end;

procedure write_data_hw(deviceid,func,bus:byte;index_:word;data:longint;datasize:byte);assembler;
  asm
    mov ah,$80
    mov al,bus
    db $66;shl ax,16

    mov ax,index_
    and ax,$fc
    mov ah,deviceid
    shl ah,3
    or  ah,func

    mov dx,$cf8
    db $66;out dx,ax

    mov dx,index_
    and dx,3
    add dx,$cfc

    db $66;mov ax,word ptr data

    cmp datasize,1
    jne @not_size_8
    out dx,al
    jmp @continue
  @not_size_8:
    cmp datasize,2
    jne @not_size_16
    out dx,ax
    jmp @continue
  @not_size_16:
    db $66;out dx,ax
  @continue:
    mov failed,false

    db $66;xor ax,ax
    mov dx,$cf8
    db $66;out dx,ax

  end;

procedure pci_present_test; assembler;
  asm
    mov ax,$b101
    int $1a
    jc  @exit

    cmp dx,$4350
    jne @exit

    mov PCIchar,al
    mov PCI_hibus,cl
    mov PCIverlo,bl
    mov PCIverhi,bh
    mov failed,false

  @exit:
  end;

procedure search_pci_device(const vendor,device,search:word;var found:word);assembler;
  asm
    mov ax,$b102
    mov cx,[device]
    mov dx,[vendor]
    mov si,[search]
    int $1a
    mov failed,true
    jc  @ret
    les di,[found]
    mov es:[di],bx
    mov failed,false
  @ret:
  end;

procedure load_irqbuff;assembler;
  const
    irq_buf_size=SizeOf(irqbuffR.IRQ_routing_table_entry_Array);
  asm
    push ds

    mov bx,0
    mov ax,seg irqbuff
    mov es,ax
    mov di,offset irqbuff
    mov word ptr es:[di+0],irq_buf_size
    lea ax,[di+6]
    mov es:[di+2],ax
    mov es:[di+4],es

    mov ax,$f000
    mov ds,ax
    mov ax,$b10e

    int $1a
    pop ds

    mov cx,word ptr es:[di]

    cmp ah,0
    jne @exit


    mov conmap,bx
    mov len,cx
    mov failed,false

  @exit:
  end;
{$EndIf}


function lookup(deviceid,func,bus:byte;index_:word) : byte;
  begin
    if usebios then
      lookup:=lookup_bios(deviceid,func,bus,index_)
    else
      lookup:=lookup_hw  (deviceid,func,bus,index_);
  end;

function lookup_dword(deviceid,func,bus:byte;index_:word) : longint;
  begin
    if usebios then
      lookup_dword:=lookup_dword_bios(deviceid,func,bus,index_)
    else
      lookup_dword:=lookup_dword_hw  (deviceid,func,bus,index_);
  end;

procedure write_dword(deviceid,func,bus:byte;index_:word;data:longint);
  begin
    if usebios then
      write_data_bios(deviceid,func,bus,index_,data,SizeOf(longint))
    else
      write_data_hw  (deviceid,func,bus,index_,data,SizeOf(longint));
  end;

procedure write_word(deviceid,func,bus:byte;index_:word;data:smallword);
  begin
    if usebios then
      write_data_bios(deviceid,func,bus,index_,data,SizeOf(smallword))
    else
      write_data_hw  (deviceid,func,bus,index_,data,SizeOf(smallword));
  end;

procedure write_byte(deviceid,func,bus:byte;index_:word;data:byte);
  begin
    if usebios then
      write_data_bios(deviceid,func,bus,index_,data,SizeOf(byte))
    else
      write_data_hw  (deviceid,func,bus,index_,data,SizeOf(byte));
  end;

procedure open_pci_access_driver;
  {$IfDef Debug_ROM}
  var
    f                   :file;
  {$EndIf Debug_ROM}
  begin

    if pci_access_driver_open then
      close_pci_access_driver;

    {$IfDef Os2}
    open_oemhlp;
    os2_read_bios;
    {$EndIf}

    {$IfDef DPMI32}
    biosf000:=Ptr($000f0000);
    {$EndIf}

    {$IfDef Win32}
    initNT_system;
    win32_read_bios;
    {$EndIf}

    {$IfDef Linux}
    linux_read_bios;
    {$EndIf}

    {$IfDef DPMI}
    asm
      mov ax,$0002              (* DPMI: segment->descriptor *)
      mov bx,$f000
      int $31
      mov Word Ptr [biosf000+0],0
      mov Word Ptr [biosf000+2],ax
    end;
    {$EndIf}

    {$IfDef MSDOS}
    biosf000:=Ptr($F000,$0000);
    {$EndIf MSDOS}

    {$IfDef Linux_OS2_Win32}
    {$IfDef Debug_ROM}
    Assign(f,'F000.BIN');
    {$I-}
    Reset(f,1);
    {$I+}
    if IOResult<>0 then
      begin
        WriteLn('Can load BIOS image file F000.BIN.');
        WriteLn('Solution: recompile without Debug_ROM;pci_Debug in Options/Compiler');
        Halt(1);
      end;
    BlockRead(f,biosf000^,SizeOf(biosf000^));
    Close(f);
    {$EndIf Debug_ROM}
    {$EndIf Linux_OS2_Win32}

    pci_access_driver_open:=true;

  end;

procedure close_pci_access_driver;
  begin
    if not pci_access_driver_open then
      Exit;

    {$IfDef Os2}
    close_oemhlp;
    Dispose(biosf000);
    {$EndIf}

    {$IfDef Win32}
    killNT_system;
    Dispose(biosf000);
    {$EndIf}

    {$IfDef Linux}
    Dispose(biosf000);
    {$EndIf}

    biosf000:=nil;
    pci_access_driver_open:=false;
  end;

end.

