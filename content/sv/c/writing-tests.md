---
title:                "Skrivande av tester"
html_title:           "C: Skrivande av tester"
simple_title:         "Skrivande av tester"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester är en viktig del av programmering. Det innebär att man skriver kod som kontrollerar att programmet gör det som det är tänkt att göra. Det är viktigt för att säkerställa att programmet fungerar korrekt och undvika buggar och fel.

## Hur man gör:

Ett enkelt sätt att skriva tester i C är att använda sig av assert-funktionen. Den används för att uttrycka förväntningar om ett specifikt uttryck. Om uttrycket är sant, så fortsätter programmet utan några problem. Om det däremot är falskt, så kommer assert-funktionen att avbryta programmet och visa ett felmeddelande. Här är ett exempel:

 ```C
 #include <stdio.h>
 #include <assert.h>

 int main()
 {
     int a = 5;
     int b = 3;
     int sum = a + b;
     
     assert(sum == 8);
     
     printf("Summan av a och b är: %d\n", sum);
     
     return 0;
 }
 ```
 
 Om allt fungerar som det ska, så kommer programmet att skriva ut "Summan av a och b är: 8". Om inte, så kommer assert-funktionen att avbryta programmet och visa ett felmeddelande.

## Djupdykning:

Att skriva tester har blivit allt viktigare inom programmering, särskilt med tanke på den ökande komplexiteten av program och system. Istället för att manuellt testa programmet steg för steg, kan man använda sig av automatiserade tester för att spara tid och resurser. Det finns även andra testramverk som kan användas för att skriva tester i C, som till exempel CUnit och Check.

## Se även:

- [C assert function](https://www.geeksforgeeks.org/using-assert-h-header-function-c/)
- [CUnit](http://cunit.sourceforge.net/) och [Check](https://libcheck.github.io/check/)
- [An overview of test-driven development in C](https://www.cloudbees.com/blog/journey-test-driven-development-c)
- [A brief history of testing in C](https://medium.com/codingthesmartway-com-blog/a-brief-history-of-testing-in-c-ed5b34e053d5)