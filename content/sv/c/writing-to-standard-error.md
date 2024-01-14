---
title:    "C: Skriva till standardfel"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför
Att skriva till standard error i C-program är ett viktigt koncept för felhantering och debugging. Genom att skriva felmeddelanden till standard error istället för standard output, kan du separera dina utskrifter och fokusera på att fånga och hantera eventuella fel som uppstår under körningen av ditt program.

## Hur man gör
För att skriva till standard error i C-program, måste du använda funktionen "fprintf()" från standardbiblioteket "stdio.h". Den här funktionen tar som argument en filpekare till den fil du vill skriva till och ett formatsträng med de önskade utskrifterna.

För att skriva ett enkelt felmeddelande till standard error skulle det se ut så här:

```C
#include <stdio.h>

int main() {
  FILE *fp = stderr;
  fprintf(fp, "Ett fel har uppstått.");
}
```
Output: Ett fel har uppstått.

I det här exemplet har vi definierat filpekaren "fp" till att peka på standard error genom att använda "stderr" som argument till "fprintf()". Sedan använder vi "fprintf()" för att skriva ut vårt felmeddelande.

## Djupdykning
För att kunna använda "fprintf()" för att skriva till standard error, måste vi förstå dess syntax ordentligt. Den tar som första argument en filpekare av typen "FILE *" som pekar på den fil du vill skriva till. Detta kan vara standard error som vi använde i exemplet ovan, eller en annan fil som du öppnar och sparar i en variabel.

Det andra argumentet är en formatsträng som innehåller det som ska skrivas ut, inklusive eventuella variabler eller symboler som "%%" för att skriva ut ett "%" tecken.

När du skriver till standard error, använd "fprintf()" istället för "printf()", men tänk på att följa samma syntax som för "printf()". Detta betyder att du kan använda %-symboler för att formatera utskriften.

## Se även
- Läs mer om "fprintf()": https://www.programiz.com/c-programming/library-function/stdio.h/fprintf
- Om grundläggande felhantering i C: https://www.codingunit.com/c-tutorial-error-handling
- Vanliga misstag att undvika i C-programmering: https://www.guru99.com/c-errors.html