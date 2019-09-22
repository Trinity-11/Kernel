@echo off

md ..\bin
md ..\bin\debug
md ..\bin\debug\roms

:start
del *.lst
64tass --m65816 kernel.asm --long-address --flat  -b --intel-hex -o kernel.hex --list=kernel.lst --labels=kernel.lbl
if errorlevel 1 goto fail

REM copy kernel.hex ..\bin\debug\roms
REM copy kernel.lst ..\bin\debug\roms

:fail
choice /m "Try again?"
if errorlevel 2 goto end
goto start

:end
echo END OF LINE
