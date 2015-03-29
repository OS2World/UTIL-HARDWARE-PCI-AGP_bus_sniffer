@echo off
call pasvpo pci M:\ /Dpci_Debug /DDebug_ROM
if [%USER%]==[Veit] copy M:\pci.exe C:\extra\pcid.exe
