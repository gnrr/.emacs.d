@echo off

set DIR=%1
set EXE=%2
set SRC=%3
set LOG=%4


cd %DIR%
%EXE% /compile:%SRC% /log /s & type %LOG%

rem echo %DIR% >  dbg.log
rem echo %EXE% >> dbg.log
rem echo %SRC% >> dbg.log
rem echo %LOG% >> dbg.log

