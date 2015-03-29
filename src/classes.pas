{ PCI Classes }

type pci_record =
  record
    class_,
    subclass,
    progif      : byte;
    cname       : pChar;
  end;

const
   pci_class_names      : array [0..$11] of pChar = (

   'Unknown',
   'Storage',
   'Network',
   'Display',
   'Multimedia',
   'Memory',
   'Bridge',
   'Simple Communication',
   'System',
   'Input',
   'Docking',
   'Processor',
   'Serial',
   'Wireless',
   'Intelligent I/O',
   'Satellite Communication',
   'En/Decryption',
   'Data Aquisition & Signal Processing'
   );

   pci_class_array : array [0..144] of pci_record = (

   (class_:$00;subclass:$00;progif:$00;cname:'Undefined'),
   (class_:$00;subclass:$01;progif:$00;cname:'VGA'),

   (class_:$01;subclass:$00;progif:$00;cname:'SCSI'),
   (class_:$01;subclass:$01;progif:$00;cname:'IDE (ATA)'), {progif: bit encoded features}
   (class_:$01;subclass:$02;progif:$00;cname:'Floppy'),
   (class_:$01;subclass:$03;progif:$00;cname:'IPI Bus'),
   (class_:$01;subclass:$04;progif:$00;cname:'RAID'),
   (class_:$01;subclass:$05;progif:$20;cname:'ATA (Single ADMA)'),
   (class_:$01;subclass:$05;progif:$30;cname:'ATA (Chained ADMA)'),
   (class_:$01;subclass:$06;progif:$00;cname:'Serial ATA'),
   (class_:$01;subclass:$06;progif:$01;cname:'Serial ATA (AHCI 1.0)'),
   (class_:$01;subclass:$07;progif:$00;cname:'Serial Attached SCSI'),
   (class_:$01;subclass:$80;progif:$00;cname:'Other'),

   (class_:$02;subclass:$00;progif:$00;cname:'Ethernet'),
   (class_:$02;subclass:$01;progif:$00;cname:'Token Ring'),
   (class_:$02;subclass:$02;progif:$00;cname:'FDDI'),
   (class_:$02;subclass:$03;progif:$00;cname:'ATM'),
   (class_:$02;subclass:$04;progif:$00;cname:'ISDN'),
   (class_:$02;subclass:$05;progif:$00;cname:'WorldFip'),
   (class_:$02;subclass:$06;progif:$00;cname:'PICMG 2.14'),
   (class_:$02;subclass:$80;progif:$00;cname:'Other'),

   (class_:$03;subclass:$00;progif:$00;cname:'VGA'),
   (class_:$03;subclass:$00;progif:$01;cname:'VGA+8514'),
   (class_:$03;subclass:$01;progif:$00;cname:'XGA'),
   (class_:$03;subclass:$02;progif:$00;cname:'3D'),
   (class_:$03;subclass:$80;progif:$00;cname:'Other'),

   (class_:$04;subclass:$00;progif:$00;cname:'Video'),
   (class_:$04;subclass:$01;progif:$00;cname:'Audio'),
   (class_:$04;subclass:$02;progif:$00;cname:'Telephony'),
   (class_:$04;subclass:$80;progif:$00;cname:'Other'),

   (class_:$05;subclass:$00;progif:$00;cname:'RAM'),
   (class_:$05;subclass:$01;progif:$00;cname:'Flash'),
   (class_:$05;subclass:$80;progif:$00;cname:'Other'),

   (class_:$06;subclass:$00;progif:$00;cname:'PCI to HOST'),
   (class_:$06;subclass:$01;progif:$00;cname:'PCI to ISA'),
   (class_:$06;subclass:$02;progif:$00;cname:'PCI to EISA'),
   (class_:$06;subclass:$03;progif:$00;cname:'PCI to MCA'),
   (class_:$06;subclass:$04;progif:$00;cname:'PCI to PCI'),
   (class_:$06;subclass:$04;progif:$01;cname:'PCI to PCI (Subtractive Decode)'),
   (class_:$06;subclass:$05;progif:$00;cname:'PCI to PCMCIA'),
   (class_:$06;subclass:$06;progif:$00;cname:'PCI to NuBUS'),
   (class_:$06;subclass:$07;progif:$00;cname:'PCI to Cardbus'),
   (class_:$06;subclass:$08;progif:$00;cname:'PCI to RACEway (Transparent)'), {progif: only bit 0}
   (class_:$06;subclass:$08;progif:$01;cname:'PCI to RACEway (End-point)'),   {progif: only bit 0}
   (class_:$06;subclass:$09;progif:$40;cname:'PCI to PCI (Primary bus to host)'),
   (class_:$06;subclass:$09;progif:$80;cname:'PCI to PCI (Secondary bus to host)'),
   (class_:$06;subclass:$0A;progif:$00;cname:'PCI to InfiBand'),
   (class_:$06;subclass:$80;progif:$00;cname:'PCI to Other'),

   (class_:$07;subclass:$00;progif:$00;cname:'Serial'),
   (class_:$07;subclass:$00;progif:$01;cname:'Serial - 16450'),
   (class_:$07;subclass:$00;progif:$02;cname:'Serial - 16550'),
   (class_:$07;subclass:$00;progif:$03;cname:'Serial - 16650'),
   (class_:$07;subclass:$00;progif:$04;cname:'Serial - 16750'),
   (class_:$07;subclass:$00;progif:$05;cname:'Serial - 16850'),
   (class_:$07;subclass:$00;progif:$06;cname:'Serial - 16950'),
   (class_:$07;subclass:$01;progif:$00;cname:'Parallel'),
   (class_:$07;subclass:$01;progif:$01;cname:'Parallel - BiDir'),
   (class_:$07;subclass:$01;progif:$02;cname:'Parallel - ECP'),
   (class_:$07;subclass:$01;progif:$03;cname:'Parallel - IEEE1284 Controller'),
   (class_:$07;subclass:$01;progif:$FE;cname:'Parallel - IEEE1284 Target'),
   (class_:$07;subclass:$02;progif:$00;cname:'Multiport Serial'),
   (class_:$07;subclass:$03;progif:$00;cname:'Hayes Compatible Modem'),
   (class_:$07;subclass:$03;progif:$01;cname:'Hayes Compatible Modem, 16450'),
   (class_:$07;subclass:$03;progif:$02;cname:'Hayes Compatible Modem, 16550'),
   (class_:$07;subclass:$03;progif:$03;cname:'Hayes Compatible Modem, 16650'),
   (class_:$07;subclass:$03;progif:$04;cname:'Hayes Compatible Modem, 16750'),
   (class_:$07;subclass:$04;progif:$00;cname:'GPIB (IEEE 488.1/2)'),
   (class_:$07;subclass:$05;progif:$00;cname:'Smart Card'),
   (class_:$07;subclass:$80;progif:$00;cname:'Other'),

   (class_:$08;subclass:$00;progif:$00;cname:'PIC'),
   (class_:$08;subclass:$00;progif:$01;cname:'ISA PIC'),
   (class_:$08;subclass:$00;progif:$02;cname:'EISA PIC'),
   (class_:$08;subclass:$00;progif:$10;cname:'I/O APIC'),
   (class_:$08;subclass:$00;progif:$20;cname:'I/O(x) APIC'),
   (class_:$08;subclass:$01;progif:$00;cname:'DMA'),
   (class_:$08;subclass:$01;progif:$01;cname:'ISA DMA'),
   (class_:$08;subclass:$01;progif:$02;cname:'EISA DMA'),
   (class_:$08;subclass:$02;progif:$00;cname:'Timer'),
   (class_:$08;subclass:$02;progif:$01;cname:'ISA Timer'),
   (class_:$08;subclass:$02;progif:$02;cname:'EISA Timer'),
   (class_:$08;subclass:$03;progif:$00;cname:'RTC'),
   (class_:$08;subclass:$03;progif:$01;cname:'ISA RTC'),
   (class_:$08;subclass:$04;progif:$00;cname:'Hot-Plug'),
   (class_:$08;subclass:$05;progif:$00;cname:'SD Host Controller (Without DMA)'),
   (class_:$08;subclass:$05;progif:$01;cname:'SD Host Controller (With DMA)'),
   (class_:$08;subclass:$80;progif:$00;cname:'Other'),

   (class_:$09;subclass:$00;progif:$00;cname:'Keyboard'),
   (class_:$09;subclass:$01;progif:$00;cname:'Pen'),
   (class_:$09;subclass:$02;progif:$00;cname:'Mouse'),
   (class_:$09;subclass:$03;progif:$00;cname:'Scanner'),
   (class_:$09;subclass:$04;progif:$00;cname:'Game Port'),
   (class_:$09;subclass:$04;progif:$10;cname:'Game Port (Legacy)'),
   (class_:$09;subclass:$80;progif:$00;cname:'Other'),

   (class_:$0a;subclass:$00;progif:$00;cname:'Generic'),
   (class_:$0a;subclass:$80;progif:$00;cname:'Other'),

   (class_:$0b;subclass:$00;progif:$00;cname:'386'),
   (class_:$0b;subclass:$01;progif:$00;cname:'486'),
   (class_:$0b;subclass:$02;progif:$00;cname:'Pentium'),
   (class_:$0b;subclass:$03;progif:$00;cname:'PentiumPro'),
   (class_:$0b;subclass:$10;progif:$00;cname:'DEC Alpha'),
   (class_:$0b;subclass:$20;progif:$00;cname:'PowerPC'),
   (class_:$0b;subclass:$30;progif:$00;cname:'MIPS'),
   (class_:$0b;subclass:$40;progif:$00;cname:'Coprocessor'),
   (class_:$0b;subclass:$80;progif:$00;cname:'Other'),

   (class_:$0c;subclass:$00;progif:$00;cname:'FireWire'),
   (class_:$0c;subclass:$00;progif:$10;cname:'OHCI FireWire'),
   (class_:$0c;subclass:$01;progif:$00;cname:'ACCESS.bus'),
   (class_:$0c;subclass:$02;progif:$00;cname:'SSA'),
   (class_:$0c;subclass:$03;progif:$00;cname:'USB (UHCI)'),
   (class_:$0c;subclass:$03;progif:$10;cname:'USB (OHCI)'),
   (class_:$0c;subclass:$03;progif:$20;cname:'USB 2.0 (EHCI)'),
   (class_:$0c;subclass:$03;progif:$80;cname:'USB'),
   (class_:$0c;subclass:$03;progif:$FE;cname:'USB Device'),
   (class_:$0c;subclass:$04;progif:$00;cname:'Fibre Channel'),
   (class_:$0c;subclass:$05;progif:$00;cname:'SMBus Controller'),
   (class_:$0c;subclass:$06;progif:$00;cname:'InfiniBand'),
   (class_:$0c;subclass:$07;progif:$00;cname:'IPMI (SMIC)'),
   (class_:$0c;subclass:$07;progif:$01;cname:'IPMI (Keyb)'),
   (class_:$0c;subclass:$07;progif:$02;cname:'IPMI (Block Tx)'),
   (class_:$0c;subclass:$08;progif:$00;cname:'SERCOS (IEC61491)'),
   (class_:$0c;subclass:$09;progif:$00;cname:'CANbus'),
   (class_:$0c;subclass:$80;progif:$00;cname:'Other'),

   (class_:$0d;subclass:$00;progif:$00;cname:'iRDA Controller'),
   (class_:$0d;subclass:$01;progif:$00;cname:'Consumer IR'),
   (class_:$0d;subclass:$10;progif:$00;cname:'RF Controller'),
   (class_:$0d;subclass:$11;progif:$00;cname:'Bluetooth'),
   (class_:$0d;subclass:$12;progif:$00;cname:'Broadband'),
   (class_:$0d;subclass:$20;progif:$00;cname:'Wireless Ethernet 802.11a'),
   (class_:$0d;subclass:$21;progif:$00;cname:'Wireless Ethernet 802.11b'),
   (class_:$0d;subclass:$80;progif:$00;cname:'Other'),

   (class_:$0e;subclass:$00;progif:$00;cname:'I2O'),
   (class_:$0e;subclass:$80;progif:$00;cname:'Other'),

   (class_:$0f;subclass:$01;progif:$00;cname:'TV'),
   (class_:$0f;subclass:$02;progif:$00;cname:'Audio'),
   (class_:$0f;subclass:$03;progif:$00;cname:'Voice'),
   (class_:$0f;subclass:$04;progif:$00;cname:'Data'),
   (class_:$0f;subclass:$80;progif:$00;cname:'Other'),

   (class_:$10;subclass:$00;progif:$00;cname:'Network'),
   (class_:$10;subclass:$10;progif:$00;cname:'Entertainment'),
   (class_:$10;subclass:$80;progif:$00;cname:'Other'),

   (class_:$11;subclass:$00;progif:$00;cname:'DPIO Modules'),
   (class_:$11;subclass:$01;progif:$00;cname:'Performance Counters'),
   (class_:$11;subclass:$10;progif:$00;cname:'Comm. Sync, Time+Freq. Measurement'),
   (class_:$11;subclass:$20;progif:$00;cname:'Management Card'),
   (class_:$11;subclass:$80;progif:$00;cname:'Other')

   );

