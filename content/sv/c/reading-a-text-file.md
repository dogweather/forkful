---
title:                "C: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil i C-programmering kan vara en användbar färdighet att ha eftersom det låter dig läsa data från en extern källa och använda den i ditt program.

## Så här gör du

Att läsa en textfil i C-programmering kräver bara några få enkla steg. Först måste du öppna filen med fopen() funktionen och förse den med filens namn och "r" -läget för att ange att filen ska läsas. 

```C

FILE *fp;
fp = fopen("textfil.txt", "r");

```

Nästa steg är att använda en loop för att läsa in filens innehåll rad för rad med hjälp av fgets() funktionen tills du når slutet av filen. Du kan sedan skriva ut varje rad till konsolen eller använda den för att utföra en viss uppgift.

```C

char line[100];

while(fgets(line, sizeof(line), fp)) {
    // Gör något med "line"
}

```

## Djupdykning

Att läsa en textfil kan också innebära att hantera eventuella fel som kan uppstå under processen. Det är viktigt att kontrollera om filen är riktig öppen innan du börjar läsa den och att stänga filen när du är klar med hjälp av fclose() funktionen.

Det kan också vara användbart att känna till olika sätt att läsa en textfil, till exempel att läsa in hela filen på en gång med hjälp av "fread()", eller att läsa in en viss mängd tecken från filen med hjälp av "fgetc()". Det är också möjligt att läsa in andra typer av filer än textfiler, som binärdata.

## Se även

För mer information om hur man läser en textfil i C-programmering, kolla in följande resurser:

- [C Programming File Handling](https://www.tutorialspoint.com/cprogramming/c_file_io.html)
- [C Graphics Tutorial](https://www.programiz.com/c-programming/c-graphics-programming)