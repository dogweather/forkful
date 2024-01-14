---
title:                "C: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att kunna skriva textfiler är en essentiell del av programmering. Det tillåter dig att spara data på ett enkelt och strukturerat sätt, och kan vara användbart i många scenarier. Till exempel kan du använda textfiler för att spara användardata, loggar eller konfigurationsfiler.

## Hur man gör det
För att skriva en textfil i C behöver du först öppna en "file stream" med hjälp av funktionen `fopen()`. Du behöver också ett filnamn och en "mode" för filen, som kan vara "r" för läsning, "w" för skrivning eller "a" för append (att lägga till data i slutet av filen). Om filen inte redan finns kommer `fopen()` att skapa den.

När filen har öppnats kan du använda funktionen `fprintf()` för att skriva data till filen. Den tar emot en "file stream", formatet för datan du vill skriva och själva datan som argument. Här är ett enkelt exempel som skriver texten "Hej världen!" till en fil som heter "hello.txt":

```C
#include <stdio.h>

int main() {
  FILE* fp = fopen("hello.txt", "w"); // Öppna filen för skrivning
  fprintf(fp, "Hej världen!"); // Skriv data till filen
  fclose(fp); // Stäng filen 
  return 0;
}
```

Om allt går som det ska, kommer en fil med namnet "hello.txt" att skapas och innehålla texten "Hej världen!".

## Djupdykning
När du skriver en textfil med `fprintf()` kan du använda speciella "format specifiers" för att tillåta olika typer av data. Till exempel kan du använda `%d` för att skriva ut ett heltal, `%f` för flyttal och `%s` för en sträng. Du kan också använda `\n` för att skapa en ny rad i filen.

Det är också viktigt att stänga filen efter att du är klar med att skriva, med hjälp av funktionen `fclose()`. Detta kommer att frigöra resurser och spara eventuella osparade ändringar i filen.

## Se även
- [Dokumentation för fopen()](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Dokumentation för fprintf()](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [Dokumentation för fclose()](https://www.tutorialspoint.com/c_standard_library/c_function_fclose.htm)