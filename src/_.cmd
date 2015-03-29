@echo off
cls

set arc=pci104vka

rem clear BP7 units/exe
for %%d in (pci_hw.tp? pci_ca.tp? redircon.tp? redirhtm.tp? rmressz.tp? pci.exe f000.bin pci.log) do if exist %%d del %%d

rem call stampdef pci.def
rem call stampdef chkpci.def

rem make DPMI32 exe and rename it
call pasvpdsp PCI ..\
if errorlevel 1 goto error
if exist ..\pci_d.exe del ..\pci_d.exe
ren ..\pci.exe pci_d.*
call copywdx ..\

rem make Win32 exe and rename it
call pasvpw PCI ..\
if errorlevel 1 goto error
if exist ..\pci_w.exe del ..\pci_w.exe
ren ..\pci.exe pci_w.*

rem make Linux exe and rename it
call pasvpl PCI ..\
if errorlevel 1 goto error
if exist ..\pci_l del ..\pci_l
ren ..\pci pci_l.*

rem make OS/2 and DOS dual exe
call dual PCI ..\
if errorlevel 1 goto error
eautil.exe ..\pci.exe pci.ea /j /o /p
call dual CHKPCI ..\
if errorlevel 1 goto error

if not [%User%]==[Veit] goto no_pack
if not [%1]==[] goto no_pack

eautil.exe ..\pcidevs.txt pcidevs.ea /j /o /p

cd ..
call genpgp

if exist M:\%arc%.arj del M:\%arc%.arj
arj.exe a M:\%arc%.arj -r -_ -xwinsvc.pas
if exist M:\%arc%.zip del M:\%arc%.zip
rearj.exe M:\%arc%.arj /tzip
set arc=

cd src

:no_pack
goto end

:error
pause

:end
