---
title:                "C: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Skapande av en temporär fil är något som många programmerare kommer stöta på under sin karriär. Det kan verka som en onödig process, men det finns många tillfällen då det kan vara användbart. I denna bloggartikel kommer vi att undersöka varför det är viktigt att kunna skapa temporära filer och hur man gör det i C-programmering.

## Hur man gör

För att skapa en temporär fil i C behöver man först inkludera biblioteket `<stdio.h>`. Sedan används funktionen `tmpfile()` för att skapa filen. Detta bör göras innan någon data skrivs till filen. Nedan följer ett exempel på hur en temporär fil kan skapas och användas:

```C
#include <stdio.h>

int main() {
  FILE *fp; // Filpekare
  fp = tmpfile(); // Skapar en temporär fil
  fprintf(fp, "Det här är en temporär fil"); // Skriver data till filen
  fclose(fp); // Stänger filen
  return 0;
}
```

Efter att filen har skapats och data har skrivits till den, kan man använda `fclose()` för att stänga filen. Det är viktigt att göra detta för att frigöra systemresurser och se till att filen inte fortsätter att användas utanför programmet.

## Djupdykning

Att skapa en temporär fil kan vara en nyttig process i många situationer. En av de vanligaste användningarna är att skriva data till en fil och sedan läsa tillbaka den senare. Detta kan vara till hjälp när man vill lagra mellanresultat och sedan använda dem för att skapa en slutlig rapport eller presentation.

En annan vanlig anledning till att skapa en temporär fil är när man har att göra med stora datamängder. Genom att skriva till en temporär fil kan man minska belastningen på systemets minne och hårddisk.

Det finns också möjlighet att ge en namngiven "prefix" till en temporär fil för att göra det lättare att hitta och arbeta med den senare. Detta kan åstadkommas genom att använda funktionen `tmpnam()` och ange prefixet som argument.

## Se även

- [C-filer tutorial](https://www.learn-c.org/en/File_I/O)
- [Skapa temporära filer i C](https://www.geeksforgeeks.org/creating-a-temporary-file-in-c/)
- [Mer om C-programmering](https://www.cprogramming.com/)