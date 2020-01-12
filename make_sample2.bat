@echo off
IF NOT EXIST lst mkdir lst
helpconv Sample2\Sample2.txt
if %errorlevel% neq 0 exit /b %errorlevel%
echo include ../Sample2/Sample2.s >Source\include_hack.s
x65 Source\instructions.s Sample2.prg -sym Sample2.sym -Ddisk_ver=0 -DborderCol=11 -DbackCol=11 -vice Sample2.vs -lst=lst\Sample2.lst
if %errorlevel% neq 0 exit /b %errorlevel%
filesize Sample2.prg
