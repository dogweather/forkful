---
title:    "C: Skapa en tillfällig fil"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför
Att skapa tillfälliga filer är ett vanligt sätt att hantera data som bara behövs för en begränsad tid. Dessa filer används ofta för att hantera temporära cache-data eller under bearbetning av större mängder data.

## Hur man gör
Att skapa en tillfällig fil i C är en relativt enkel process. Först behöver du inkludera standardbiblioteket `stdio.h` och biblioteket `stdlib.h` som innehåller funktioner för att hantera filer.
```C
#include <stdio.h>
#include <stdlib.h>
```
Sedan behöver vi skapa en filpekare som kommer att användas för att manipulera vår tillfälliga fil. Vi använder funktionen `tmpfile()` för att skapa en tillfällig fil som sparar data i det temporära katalogen.
```C
FILE *fp = tmpfile();
```
Nu kan vi skriva till filen genom att använda `fprintf()` och läsa från den genom att använda `fscanf()`.
```C
// Skriv till filen
fprintf(fp, "Det här är en tillfällig fil.\n");

// Läs från filen
char buffer[50];
fscanf(fp, "%s", buffer);
printf("Innehåll: %s", buffer);
```
Slutligen behöver vi stänga filen när vi är klara med den genom att använda `fclose()`.
```C
fclose(fp);
```

## Djupdykning
Att skapa tillfälliga filer är ett smidigt sätt att hantera temporära data utan att förstöra befintliga filer eller använda onödigt mycket diskutrymme. Så vad händer egentligen bakom kulisserna när vi använder `tmpfile()`?

När funktionen `tmpfile()` anropas skapas en temporär fil i det temporära katalogen. Denna katalog är specifik för varje operativsystem men kan ofta hittas i `/tmp` eller `C:\Temp`. Detta innebär att filen kommer att raderas automatiskt när programmet avslutas.

En annan fördel med att använda `tmpfile()` är att filen automatiskt tilldelas ett unikt namn, vilket minskar risken för namnkonflikter med befintliga filer.

## Se även
- [Wikipedia: Temporary file](https://sv.wikipedia.org/wiki/Tempor%C3%A4r_fil)
- [C Programming - File I/O](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)