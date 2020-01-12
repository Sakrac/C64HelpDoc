@echo off
IF NOT EXIST lst mkdir lst
helpconv Sample\help.txt
if %errorlevel% neq 0 exit /b %errorlevel%
x65 Source\instructions.s instructions.prg -sym instructions.sym -Ddisk_ver=0 -vice instructions.vs -lst=lst\instructions.lst
if %errorlevel% neq 0 exit /b %errorlevel%
filesize instructions.prg
