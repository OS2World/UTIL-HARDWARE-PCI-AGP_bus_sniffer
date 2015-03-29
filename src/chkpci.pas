{&Use32+}
{$I+}
program chkpci;


uses
  {$IfNDef VirtualPascal}
  {newdelay, - we do not Delay}
  {$EndIf}
  Crt,
  RedirCon;



{

Totals up and displays statistics on all data

Checks PCIDEVS.TXT for the following errors:

Unrecognised lines

Bad formatting of V, D, O, S, R, X entries;
 - formatting tab character(s) missing
 - ID digits wrong case or out of range
 - description at least 2 characters long

Wrong numeric ordering of V, D, O, S, R entries

Duplicates of V, D, O, S, R entries

ASCII charset
}




var
  fi    : text;
  fs    : file of byte;

  s     : string;



  size,

  cvt,
  cven,
  cdev,
  coem,
  csub,
  crev,

  comment,
  blank,
  ven,
  dev,
  OEM,
  Sub,
  rev   : longint;


  junk,
  x,
  p,
  i     : integer;

  t,
  f     : boolean;




function valdigits(const d:string) : boolean;
begin
  p:=0;

  for i:=1 to Length(d) do
    if d[i] in ['0'..'9','A'..'F'] then Inc(p);

  valdigits:=(p<>Length(d));
end;


function cvtb(const b:byte) : byte;
begin
  if b>9 then cvtb:=b+ord('A')-10 else cvtb:=b+ord('0');
end;

function wrhexw(const wor:word): string;
begin
  wrhexw:=chr(cvtb(wor shr 12))+chr(cvtb((wor shr 8) and $f))+chr(cvtb((wor shr 4) and $f))+chr(cvtb(wor and $f));
end;




begin
  if ioredirected then
  begin
    assign(output,'');
    rewrite(output);
  end else clrscr;

  writeln('CHKPCI Version 0.02aá');
  writeln;

  assign(fi,'PCIDEVS.TXT');
  reset(fi);
  assign(fs,'PCIDEVS.TXT');
  reset(fs);
  size:=filesize(fs);
  close(fs);

  writeln('PCIDEVS.TXT   : ',size/1024:5:1,'KiB');
  writeln;


  comment:=0;
  blank:=0;
  ven:=0;
  dev:=0;
  oem:=0;
  sub:=0;
  rev:=0;
  x:=0;


  cven:=-1;
  cdev:=-1;
  coem:=-1;
  csub:=-1;
  crev:=-1;


  while not eof(fi) do
  begin
    readln(fi,s);
    t:=false;

    if length(s)=0 then
    begin
      Inc(blank);
      t:=true;
    end;

    for i:=1 to Length(s) do
      if not (s[i] in [^I,' '..#$7f]) then
        begin
          WriteLn('Notice: line has non-ASCII characters: "',s,'"');
          Break;
        end;

    if not t then if s[1]=';' then
    begin
      Inc(comment);
      t:=true;
    end;

    if not t then if s[1]='V' then
    begin
      f:=false;
      if s[2]<>#9 then f:=true;
      if s[7]<>#9 then f:=true;
      if valdigits(copy(s,3,4)) then f:=true;
      if f then writeln('Vendor Entry Error : ',copy(s,1,52));
      t:=true;
      Inc(ven);

      val(('$'+copy(s,3,4)),cvt,junk);
      if cvt<cven then writeln('VENDOR ',copy(s,3,4),' out of order');
      if cvt=cven then writeln('VENDOR ',copy(s,3,4),' duplicated');

      cven:=cvt;
      cdev:=-1;

    end;

    if not t then if s[1]='D' then
    begin
      f:=false;
      if length(s)<8 then f:=true;
      if s[2]<>#9 then f:=true;
      if s[7]<>#9 then f:=true;
      if valdigits(copy(s,3,4)) then f:=true;
      if f then writeln('Device Entry Error : ',copy(s,1,52));
      t:=true;
      Inc(dev);

      val(('$'+copy(s,3,4)),cvt,junk);
      if cvt<cdev then writeln('DEVICE ',copy(s,3,4),', (Vendor ',wrhexw(cven),') out of order');
      if cvt=cdev then writeln('DEVICE ',copy(s,3,4),', (Vendor ',wrhexw(cven),') duplicated');
      cdev:=cvt;
      coem:=-1;
      crev:=-1;
    end;


    if not t then if s[1]='O' then
    begin
      f:=false;
      if length(s)<8 then f:=true;
      if s[2]<>#9 then f:=true;
      if s[7]<>#9 then f:=true;
      if valdigits(copy(s,3,4)) then f:=true;
      if f then writeln('OEM Entry Error    : ',copy(s,1,52));
      t:=true;
      Inc(oem);

      val(('$'+copy(s,3,4)),cvt,junk);
      if cvt<coem then writeln('OEM ',copy(s,3,4),' (Device ',wrhexw(cdev),', Vendor ',wrhexw(cven),') out of order');
      if cvt=coem then writeln('OEM ',copy(s,3,4),' (Device ',wrhexw(cdev),', Vendor ',wrhexw(cven),') duplicated');
      coem:=cvt;
      csub:=-1;
    end;

    if not t then if s[1]='S' then
    begin
      f:=false;
      if length(s)<8 then f:=true;
      if s[2]<>#9 then f:=true;
      if s[7]<>#9 then f:=true;
      if valdigits(copy(s,3,4)) then f:=true;
      if f then writeln('Subsys Entry Error : ',copy(s,1,52));
      t:=true;
      Inc(sub);

      val(('$'+copy(s,3,4)),cvt,junk);
      if cvt<csub then writeln('SUBSYS ',copy(s,3,4)
        ,' (OEM ',wrhexw(coem),' Device ',wrhexw(cdev),', Vendor ',wrhexw(cven),') out of order');
      if cvt=csub then writeln('SUBSYS ',copy(s,3,4)
        ,' (OEM ',wrhexw(coem),' Device ',wrhexw(cdev),', Vendor ',wrhexw(cven),') duplicated');
      csub:=cvt;
    end;

    if not t then if s[1]='R' then
    begin
      f:=false;
      if length(s)<6 then f:=true;
      if s[2]<>#9 then f:=true;
      if s[5]<>#9 then f:=true;
      if valdigits(copy(s,3,2)) then f:=true;
      if f then writeln('Rev Entry Error    : ',copy(s,1,52));
      t:=true;
      Inc(rev);

      val(('$'+copy(s,3,2)),cvt,junk);
      if cvt<crev then writeln('REV ',copy(s,3,2),' (SUBSYS ',wrhexw(csub),' OEM ',wrhexw(coem)
        ,' Device ',wrhexw(cdev),', Vendor ',wrhexw(cven),') out of order');
      if cvt=crev then writeln('REV ',copy(s,3,2),' (SUBSYS ',wrhexw(csub),' OEM ',wrhexw(coem)
        ,' Device ',wrhexw(cdev),', Vendor ',wrhexw(cven),') duplicated');
      crev:=cvt;

    end;

    if not t then if s[1]='X' then
    begin
      f:=false;
      if length(s)<12 then f:=true;
      if s[2]<>#9 then f:=true;
      if s[11]<>#9 then f:=true;
      if valdigits(copy(s,3,8)) then f:=true;
      if f then writeln('X Entry Error      : ',copy(s,1,52));
      t:=true;
      Inc(x);

    end;




    if t=false then writeln('Unknown line   : ',copy(s,1,52));

  end;
  close(fi);


  writeln;
  writeln('Found and checked ',ven+dev+oem+sub+rev+x,' entries');
  writeln;
  writeln('Vendors       : ',ven);
  writeln('Devices       : ',dev);
  writeln('OEM IDs       : ',oem);
  writeln('Subsystem IDs : ',sub);
  writeln('Revision IDs  : ',rev);
  writeln('X Codes       : ',x);
  writeln;
  writeln('Comment Lines : ',comment);
  writeln('Blank Lines   : ',blank);

end.
