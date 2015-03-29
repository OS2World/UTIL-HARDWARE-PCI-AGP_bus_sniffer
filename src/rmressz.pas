{&Use32+}
unit rmressz; { ask OS/2 Resource Manager about resource size }

interface

const
  ResourceMem           =false;
  ResourceIO            =true;

function ResourceSize(const address:longint;const is_io:boolean):longint;

implementation

{$IfDef Os2}

uses
  Os2Def,
  Os2Base;

const
  CAT_RM                        =$80;           // Resource Manager Category
  FUNC_RM_GET_NODEINFO          =$01;           // Get RM Structure
  FUNC_RM_ENUM_NODES            =$02;           // Get Physical Topology
  FUNC_RM_ENUM_DRIVERS          =$03;           // Get DriverHandles

  RM_COMMAND_PHYS               =0;
  RM_COMMAND_LOG                =1;
  RM_COMMAND_DRVR               =2;
  RM_COMMAND_PREVIOUS_DETECT    =3;
  RM_COMMAND_CURRENT_DETECT     =4;


  HANDLE_PHYS_TREE              =$ffff8000;
  HANDLE_SYS_TREE               =$ffff8001;
  HANDLE_DEFAULT_SYSBUS         =$ffff8002;
  HANDLE_X_BUS                  =$ffff8003;
  HANDLE_PCI_BUS                =$ffff8004;
  HANDLE_PREVIOUS_DETECTED      =$ffff8005;
  HANDLE_CURRENT_DETECTED       =$ffff8006;

  RM_IDTYPE_RM                  = 0;            // Resource Manager Internal
  RM_IDTYPE_EISA                = 1;            // EISA (Also ISA PNP)
  RM_IDTYPE_PCI                 = 2;            // PCI
  RM_IDTYPE_LEGACY              = 3;            // LEGACY

  RMTYPE_ADAPTER                =$0000;
  RMTYPE_DEVICE                 =$0001;
  RMTYPE_LDEV                   =$0002;
  RMTYPE_SYSNAME                =$0003;
  RMTYPE_DRIVER                 =$0004;
  RMTYPE_DETECTED               =$0005;
  RMTYPE_RESOURCE               =$0006;

  RS_TYPE_IO                    =1;
  RS_TYPE_IRQ                   =2;
  RS_TYPE_MEM                   =3;
  RS_TYPE_DMA                   =4;
  RS_TYPE_TIMER                 =5;
  RS_TYPE_NEW                   =$ffff;

  MAX_RESOURCES                 =10;
  MAX_TEXT_DATA                 =128;


type
  T_RMHANDLE                    =ULONG;
  T_DEVID                       =ULONG;
  T_VENDID                      =ULONG;
  T_SERNUM                      =ULONG;
  T_IDTYPE                      =ULONG;

  T_DATESTAMP                   =
    packed record
      Year                      :USHORT;
      Month                     :Byte;
      Day                       :Byte;
    end;

  T_IORESOURCE                  =
    packed record
      BaseIOPort                :USHORT;
      NumIOPorts                :USHORT;
      IOFlags                   :USHORT;
      IOAddressLines            :USHORT;
    end;

  T_MEMResource                 =
    packed record
      MemBase                   :ULONG;
      MemSize                   :ULONG;
      MemFlags                  :USHORT;
      ReservedAlign             :USHORT;
    end;

  T_ADJUNCT                     =
    packed record
      //<<
    end;

  T_RESOURCESTRUCT              =
    packed record
      res                       :
        packed record
          case ResourceType         :ULONG of
            RS_TYPE_IO:(IORESOURCE  :T_IOResource);
    //      RS_TYPE_IRQ:IRQRESOURCE :T_IRQResource);
            RS_TYPE_MEM:(MEMRESOURCE:T_MEMResource);
    //      RS_TYPE_DMA:(DMARESOURCE: T_DMAResource);
    //      RS_TYPE_TIMER:(TMRRESOURCE:T_TMRResource);
         end;
      Reserved:ULONG;
    end;

  T_ADAPTERSTRUCT               =
    packed record
      AdaptDescriptName         :pChar;
      AdaptFlags                :USHORT;
      BaseType                  :USHORT;
      SubType                   :USHORT;
      InterfaceType             :USHORT;
      HostBusType               :USHORT;
      HostBusWidth              :USHORT;
      pAdjunctList              :^T_ADJUNCT;
      Reserved                  :ULONG;
    end;

  T_DEVICESTRUCT                =
    packed record
      DevDescriptName           :pChar;
      DevFlags                  :USHORT;
      DevType                   :USHORT;
      pAdjunctList              :^T_ADJUNCT;
    end;

  T_DRIVERSTRUCT                =
    packed record
      DrvrName                  :pChar;
      DrvrDescript              :pChar;
      VendorName                :pChar;
      MajorVer                  :Byte;
      MinorVer                  :Byte;
      Date                      :T_DATESTAMP;
      DrvrFlags                 :USHORT;
      DrvrType                  :USHORT;
      DrvrSubType               :USHORT;
      DrvrCallback              :pointer;
    end;

  T_DETECTEDSTRUCT              =
    packed record
      DetectDescriptName        :pChar;
      DetectFlags               :USHORT;
      IDType                    :T_IDTYPE;
      DeviceID                  :T_DEVID;
      FunctionID                :T_DEVID;
      CompatibleID              :T_DEVID;
      pAdjunctList              :^T_ADJUNCT;
      VendorID                  :T_VENDID;
      SerialNumber              :T_SERNUM;
    end;

  T_RESOURCELIST                =
    packed record
      Count                     :ULONG;
      Resource                  :array[1..MAX_RESOURCES] of T_RESOURCESTRUCT;
    end;

  T_RM_NODE                       =
    packed record
      VersionInfo               :ULONG;
      NodeType                  :ULONG;
      DriverHandle              :T_RMHANDLE;
      pRMNode                   :
        packed record
          case integer of
            0:(pAdapterNode         :^T_ADAPTERSTRUCT);
            1:(pDeviceNode          :^T_DEVICESTRUCT);
    //      2:(pLDevNode            :^T_LDEVSTRUCT);
    //      3:(pSysNameNode         :^T_SYSNAMESTRUCT);
    //      4:(pDriverNode          :^T_DRIVERSTRUCT);
            5:(pDetectedNode        :^T_DETECTEDSTRUCT);
    //      6:(pResourceNode        :^T_RESOURCESTRUCT);
        end;
      pResourceList             :^T_RESOURCELIST;
      data_buffer               :array[1..SizeOf(T_ADAPTERSTRUCT)+MAX_TEXT_DATA
                                         +SizeOf(T_DRIVERSTRUCT )+MAX_TEXT_DATA
                                         +SizeOf(T_RESOURCESTRUCT)*MAX_RESOURCES] of byte;
    end;

  T_NODEENTRY                   =
    packed record
      RMHandle                  :T_RMHANDLE;
      Depth                     :ULONG;
    end;

  T_RM_ENUMNODES_PARM           =
    packed record
      Command                   :smallword;
    end;

  T_RM_ENUMNODES_DATA1          =
    packed record
      NumEntries                :ULONG;
      NodeEntries               :array[1..1] of T_NODEENTRY;
    end;

  T_RM_ENUMNODES_DATAX          =
    packed record
      NumEntries                :ULONG;
      NodeEntries               :array[1..10000] of T_NODEENTRY;
    end;

  T_RM_GETNODE_PARM             =
    packed record
      RMHandle                  :T_RMHANDLE;
      Linaddr                   :ULONG;
    end;

  T_RM_GETNODE_DATA             =
    packed record
      RMNodeSize                :ULONG;
      RMNode                    :T_RM_NODE;
    end;


var
  RM_ENUMNODES_PARM             :T_RM_ENUMNODES_PARM;
  RM_ENUMNODES_DATA1            :T_RM_ENUMNODES_DATA1;
  RM_ENUMNODES_DATAX            :^T_RM_ENUMNODES_DATAX;
  RM_GETNODE_PARM               :T_RM_GETNODE_PARM;
  RM_GETNODE_DATA               :T_RM_GETNODE_DATA;

function ResourceSize(const address:longint;const is_io:boolean):longint;


  var
    resource_sys        :longint;

    procedure ResourceSizeH(const RMHandle:T_RMHANDLE);
      var
        rc              :longint;
        para_size       :longint;
        data_size       :longint;
        i               :word;
      begin

        RM_GETNODE_PARM.RMHandle:=RMHandle;
        RM_GETNODE_PARM.Linaddr:=Ofs(RM_GETNODE_DATA);
        RM_GETNODE_DATA.RMNodeSize:=SizeOf(RM_GETNODE_DATA.RMNode);
        para_size:=SizeOf(RM_GETNODE_PARM);
        data_size:=SizeOf(RM_GETNODE_DATA);


        rc:=DosDevIOCtl(resource_sys,CAT_RM,FUNC_RM_GET_NODEINFO,
              @RM_GETNODE_PARM,para_size,@para_size,
              @RM_GETNODE_DATA,data_size,@data_size);
        if rc=0 then
          with RM_GETNODE_DATA.RMNode do
            if Assigned(pResourceList) then
              with pResourceList^ do
                for i:=1 to Count do
                  with Resource[i].res do
                    case ResourceType of
                      RS_TYPE_IO:
                        if is_io then
                          with IORESOURCE do
                            if address=BaseIOPort then
                              Result:=NumIOPorts;
                      RS_TYPE_MEM:
                        if not is_io then
                          with MEMRESOURCE do
                            if address=MemBase then
                              Result:=MemSize;

                    end;

      end;


    procedure ResourceSizeStart(RM_COMMAND_:longint);
      var
        rc              :longint;
        para_size       :longint;
        data_size       :longint;
        i               :word;
      begin

        RM_ENUMNODES_PARM.Command:=RM_COMMAND_;
        RM_ENUMNODES_DATA1.NumEntries:=High(RM_ENUMNODES_DATA1.NodeEntries);
        para_size:=SizeOf(RM_ENUMNODES_PARM);
        data_size:=SizeOf(RM_ENUMNODES_DATA1);

        rc:=DosDevIOCtl(resource_sys,CAT_RM,FUNC_RM_ENUM_NODES,
              @RM_ENUMNODES_PARM ,para_size,@para_size,
              @RM_ENUMNODES_DATA1,data_size,@data_size);
        if (rc=0) or (rc=$ff0c) then
          if RM_ENUMNODES_DATA1.NumEntries>0 then
            begin

              RM_ENUMNODES_PARM.Command:=RM_COMMAND_;
              para_size:=SizeOf(RM_ENUMNODES_PARM);
              data_size:=SizeOf(RM_ENUMNODES_DATA1.NumEntries)
                        +SizeOf(RM_ENUMNODES_DATA1.NodeEntries)
                        *       RM_ENUMNODES_DATA1.NumEntries;
              GetMem(RM_ENUMNODES_DATAX,data_size);
              RM_ENUMNODES_DATAX^.NumEntries:=RM_ENUMNODES_DATA1.NumEntries;

              if DosDevIOCtl(resource_sys,CAT_RM,FUNC_RM_ENUM_NODES,
                   @RM_ENUMNODES_PARM ,para_size,@para_size,
                    RM_ENUMNODES_DATAX,data_size,@data_size)=0 then
                with RM_ENUMNODES_DATAX^ do
                  for i:=1 to NumEntries do
                    with NodeEntries[i] do
                      begin
                        ResourceSizeH(RMHandle);
                        if Result<>0 then Break;
                      end;

              Dispose(RM_ENUMNODES_DATAX);
            end;
      end;

  begin

    Result:=0;
    if address=0 then Exit;

    if SysFileOpen('RESMGR$',$40,resource_sys)<>No_Error then Exit;

      ResourceSizeStart(RM_COMMAND_PHYS          );
    if Result=0 then
      ResourceSizeStart(RM_COMMAND_CURRENT_DETECT);
    if Result=0 then
      ResourceSizeStart(RM_COMMAND_PREVIOUS_DETECT);

    SysFileClose(resource_sys);
  end;

{$Else} {not implemented for DOS, Linux, Windows}

function ResourceSize(const address:longint;const is_io:boolean):longint;
  begin
    ResourceSize:=0;
  end;

{$EndIf}

end.

