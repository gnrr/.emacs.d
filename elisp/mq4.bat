@echo off

set TYPE=%1
set DIR=%2
set EXE=%3
set SRC=%4
set LOG=%5

rem echo %TYPE% > dbg.log
rem echo %DIR% >> dbg.log
rem echo %EXE% >> dbg.log
rem echo %SRC% >> dbg.log
rem echo %LOG% >> dbg.log

cd %DIR%

if %TYPE%==flymake (
    %EXE% /compile:%SRC% /log /s & type %LOG%
) else if %TYPE%==compile (
    %EXE% /compile:%SRC% && exit /b 0 || exit /b 1
) else (
    exit /b 1
)
