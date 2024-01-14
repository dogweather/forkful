---
title:    "C: Att läsa en textfil"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

Att kunna läsa från en textfil är en viktig del av programmering. Det gör det möjligt för användaren att spara och manipulera data på ett enkelt och effektivt sätt. Om du är ny inom C-programmering kan det verka lite förvirrande, men oroa dig inte - vi kommer guida dig genom processen!

## Såhär gör man

Att läsa från en textfil i C är en relativt enkel process. Först måste du öppna filen med hjälp av `fopen()` -funktionen. Du måste ange både filnamnet och läge (t.ex. "läsa" eller "skriva") som argument. Om du vill läsa från filen, måste du använda läget "rt". Till exempel: 
```C
FILE *file;
file = fopen("mitttextdokument.txt", "rt");
```
Nästa steg är att läsa informationen från filen. Du kan använda `fgetc()` -funktionen för att läsa ett tecken åt gången eller `fgets()` -funktionen för att läsa en hel rad. Till exempel:
```C
char c;
c = fgetc(file); //läser första tecknet från filen
char str[100];
fgets(str, 100, file); //läser en hel rad från filen och lagrar den i en sträng
```
För att kontrollera om filen verkligen lästes in, kan du använda `feof()` -funktionen för att testa om filen har nått slutet. Om du vill stänga filen efter att du har läst från den, kan du använda `fclose()` -funktionen. Till exempel:
```C
fclose(file); //stänger filen
```

## Djupdykning

En viktig sak att komma ihåg när du läser från en textfil i C är att formatteringen är viktig. Om du till exempel försöker läsa en teckensträng som är längre än den array du lagrade den i, kan du få oväntade resultat. Detta kan också orsaka problem när du arbetar med flera plattformar.

En annan faktor att tänka på är att när du öppnar en fil måste du också stänga den när du är klar. Om du lämnar filen öppen, kan det leda till minnesläckor och andra problem. Det är alltid en god idé att följa upp `fopen()` med en `fclose()`.

## Se även

För mer detaljerad information om läsning och skrivning av filer i C, rekommenderar vi följande resurser:

- [C-filhantering](https://www.programiz.com/c-programming/c-file-input-output)
- [Filhantering i C - Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [C-fil I/O - Geeks for Geeks](https://www.geeksforgeeks.org/basics-file-handling-c/)

Vi hoppas att denna guide har varit till hjälp för dig när det gäller att läsa från en textfil i C. Lycka till med dina framtida programmeringsprojekt!