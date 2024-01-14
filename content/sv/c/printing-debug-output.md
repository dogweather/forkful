---
title:    "C: Utskrift av felsökningsutdata"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

I många situationer kan det vara användbart för programmerare att skriva ut debug-utdata. Genom att skriva ut variabler och andra värden kan man enklare felsöka och förstå hur koden fungerar.

## Så här gör du

För att skriva ut debug-utdata i C-programmering kan du använda funktionen `printf()`. Du behöver bara ange värdet eller variabeln du vill skriva ut inom citattecken och placera en skrivningsindikator (%d, %f, %c, etc) före för att ange vilken datatyp som ska skrivas ut. Här är ett exempel:

```C
int num = 5; 
printf("Värdet på num är %d", num); 
```

Detta kommer att skriva ut "Värdet på num är 5" i terminalen.

## Djupdykning

För att få mer detaljerad information i din debug-utdata kan du använda `fprintf()`-funktionen. Den tar ett extra argument som anger vilken fil du vill skriva ut till. Detta är användbart om du vill spara din debug-utdata till en fil för senare användning. Här är en kodexempel:

```C
FILE *fp;
fp = fopen("debug.txt", "w");
fprintf(fp, "Värdet på num är %d", num);
fclose(fp);
```

Detta kommer att skapa en fil som heter "debug.txt" som innehåller "Värdet på num är 5".

## Se även

- [C-programmering för nybörjare](https://www.codingame.com/playgrounds/14213/introduction-to-c)
- [Debugging tips och tricks](https://stackify.com/top-20-c-debugging-tips/)

* Vill du lära dig mer om C-programmering? Kolla in vår artikelserie här på bloggen "Introduktion till C-programmering".