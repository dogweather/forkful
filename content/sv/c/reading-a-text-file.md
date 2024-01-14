---
title:                "C: Läsa en textfil"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att läsa en textfil är en vanlig uppgift för programmerare och användbar för många olika ändamål. Det kan vara att läsa in användardata, behandla stora mängder information eller bara skapa en textbaserad applikation. Oavsett vad anledningen är, kan det vara en användbar kunskap för programmerare att veta.

## Hur man gör
För att läsa en textfil i C, behöver du först öppna filen med hjälp av ```fopen()``` funktionen. Du behöver också ange om du vill läsa, skriva eller lägga till i filen. Om du bara vill läsa innehållet i filen, kan du använda läget "r". Sedan använder du ```fscanf()``` för att läsa innehållet rad för rad tills du når slutet av filen.

```c
#include <stdio.h>

int main(void)
{
    FILE *fp; //variabel för att hålla filreferensen 
    char buffer[255]; //för att hålla en rad text från filen

    fp = fopen("textfil.txt", "r"); //öppna filen i läge "r"

    if (fp == NULL) //om filen inte kan öppnas, avbryt programmet
    {
        printf("Filen kunde inte öppnas.");
        return 1;
    }

    while (fscanf(fp, "%s", buffer) != EOF) //loopa tills slutet av filen är nått med hjälp av fscanf
    {
        printf("%s\n", buffer); //skriv ut innehållet från raden i filen
    }

    fclose(fp); //stäng filen när du är klar

    return 0;
}
```

Om du till exempel har en textfil som innehåller:

> Hej
> Världen 
> Detta är en textfil

Så kommer utmatningen att bli:

> Hej
> Världen
> Detta
> är
> en
> textfil

## Djupdykning
När du läser en textfil i C, kan du också hantera specialtecken som radbrytningar och mellanslag genom att använda ```fgets()``` och ```sscanf()``` istället för ```fscanf()```. Du kan också använda ```fprintf()``` för att skriva till en textfil.

Det är också viktigt att kontrollera om filen öppnas korrekt genom att använda en if-sats. Om filen inte kan öppnas, så bör programmet avslutas för att undvika eventuella felmeddelanden eller felaktig datainsamling.

## Se även
- [C File Input/Output](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [C String Functions](https://www.programiz.com/c-programming/library-function/string.h)