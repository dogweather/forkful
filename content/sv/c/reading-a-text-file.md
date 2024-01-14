---
title:    "C: Läsning av en textfil"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en viktigt del av att lära sig programmera i C. Genom att kunna läsa in data från en fil, kan du skapa mer dynamiska och användbara program. Det är också ett bra sätt att öva på filhantering i C.

## Så här gör du

För att läsa en textfil i C, behöver du först öppna filen med hjälp av fopen() funktionen. Detta låter dig öppna filen i ett "read-only" läge, vilket innebär att filen bara kan läsas och inte ändras. För att öppna filen i "read-only" läge, använd följande syntax:

```C
FILE *fp; // Deklarera filpekaren
fp = fopen("filnamn.txt", "r"); // Öppna filen i läge "read-only"
```

Efter att ha öppnat filen kan du sedan läsa innehållet rad för rad med hjälp av fgets() funktionen. Denna funktion läser in en rad av text från filen och sparar den i en strängvariabel. Du kan sedan använda printf() funktionen för att skriva ut raden på skärmen.

```C
char rad[100]; // Skapa en strängvariabel för att lagra raden
while (fgets(rad, 100, fp)) // Loopa tills alla rader har lästs
{
    printf("%s", rad); // Skriv ut raden på skärmen
}
```

Om du vill läsa en specifik del av filen kan du använda fseek() funktionen för att navigera till en viss position i filen. Detta görs genom att först sätta läge på filpekaren med hjälp av fseek() funktionen och sedan läsa in önskad data med hjälp av fgets().

```C
fseek(fp, 16, SEEK_SET); // Sätta läge på filpekaren till teckennummer 16
fgets(rad, 100, fp); // Läsa in nästa rad efter läget
printf("%s", rad); // Skriv ut raden på skärmen
```

## Djupdykning

När du läser en textfil i C, är det viktigt att förstå hur olika teckenkodningar kan påverka inläsningen av filen. C använder standardteckenkodningen ASCII, vilket innebär att vissa specialtecken (som åäö) kanske inte läses in korrekt om filen är kodad på ett annat sätt. För att lösa detta kan du använda en textredigerare som kan konvertera filen till ASCII-kodning innan du läser in den.

En annan sak att tänka på är att textfiler ofta innehåller radbrytningar som skiljer sig åt beroende på vilket operativsystem filen är skapad på. I Windows använder man till exempel \r\n, medan det i Unix/Linux används \n. Detta kan leda till problem om du inte hanterar radbrytningarna på rätt sätt i din kod.

## Se även

- [Dokumentation om filhantering i C (på svenska)](https://www.programiz.com/c-programming/c-file-input-output)
- [Guide för att läsa och skriva filer i C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [C-funktioner för filhantering (på svenska)](https://www.webblabbet.se/funktioner-for-filhantering-pa-c/)