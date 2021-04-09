@echo off

set TYPE=%1
set DIR=%2
set EXE=%3
set SRC=%4
set INC=%5
set LOG=%6

cd %DIR%

if %TYPE%==flymake (
    call :exec_flymake
) else if %TYPE%==compile (
    call :exec_compile
) else (
    call :exec_other
)

exit /b %errorlevel%


:exec_flymake
    %EXE% /compile:%SRC% /include:%INC% /log /s & type %LOG%
    exit /b 0

:exec_compile
    %EXE% /compile:%SRC% /include:%INC%

    set EX4=%SRC:.mq4=.ex4%

    if exist %EX4% (
        exit /b 0
    ) else (
        exit /b 255
    )

:exec_other
    exit /b 1
