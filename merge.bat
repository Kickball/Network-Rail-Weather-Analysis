@echo off
title Merge Text Files
color 9f
echo Welcome to the program!
set /p loc=Please enter the location of folder where all txt files are located:
cd %loc%
set /p name=Type the name of new merged file (without adding ".txt"):
echo Press enter button to continue!
pause>nul
copy /b *.txt %name%.txt
echo Done!
pause>nul
echo Press any key to exit!
pause>nul
exit
