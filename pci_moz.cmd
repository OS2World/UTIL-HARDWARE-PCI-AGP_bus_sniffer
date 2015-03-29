@echo off
if     [%WINDIR%]==[] pci.exe   -b -m -p -r > %tmp%\pci.htm
if not [%WINDIR%]==[] pci_w.exe -b -m -p -r > %tmp%\pci.htm

call mozilla file://%tmp%\pci.htm
if not errorlevel 1 goto ende

call ff file://%tmp%\pci.htm
if not errorlevel 1 goto ende

if     [%WINDIR%]==[] call  rexxtry rc=SysSetObjectData('%tmp%\pci.htm','OPEN=DEFAULT')
if not [%WINDIR%]==[] start %tmp%\pci.htm
if not errorlevel 2 goto ende

echo can not find browser, please adapt %0..
pause

:ende
