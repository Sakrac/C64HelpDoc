@echo off
IF NOT EXIST lst mkdir lst
helpconv Sample\help.txt
if %errorlevel% neq 0 exit /b %errorlevel%
echo include ../Sample/help.s >Source\include_hack.s
x65 Source\instructions.s Sample.prg -sym Sample.sym -Ddisk_ver=0 -DborderCol=6 -DbackCol=6 -vice Sample.vs -lst=lst\Sample.lst
if %errorlevel% neq 0 exit /b %errorlevel%
filesize Sample.prg
