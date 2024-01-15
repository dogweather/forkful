---
title:                "Läsa en textfil"
html_title:           "C: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en grundläggande funktion inom programmering och är ofta en nyckelkomponent i att hantera data och information. Genom att läsa en textfil kan du få tillgång till och manipulera information som annars skulle vara svåråtkomlig.

## Hur man gör det

För att läsa en textfil i C kan du använda funktionen `fopen()` för att öppna filen och sedan använda `fscanf()` för att läsa in data från filen. Här är ett exempel som öppnar en fil med namnet "textfil.txt" och skriver ut innehållet rad för rad:

```
#include <stdio.h>

int main() {
    FILE *fp;

    // Öppna filen för läsning
    fp = fopen("textfil.txt", "r");

    // Läs in och skriv ut rad för rad
    char line[100];
    while (fscanf(fp, "%[^\n]\n", line) != EOF) {
        printf("%s\n", line);
    }

    // Stäng filen
    fclose(fp);
    return 0;
}
```

Om filen "textfil.txt" innehåller följande:

```
Hej!
Det här är en textfil.
```

Så kommer outputen av programmet att vara:

```
Hej!
Det här är en textfil.
```

## Djupdykning

För att förstå mer om hur textfiler läses i C är det viktigt att förstå formatsträngen som används i `fscanf()`. I exemplet ovan används `"%[^\n]\n"` som betyder att funktionen kommer att läsa in allt fram till ett radbrytningstecken (`\n`) och sedan även läsa in och kasta bort radbrytningen. Detta gör att vi kan läsa en hel rad åt gången.

En annan viktig aspekt att tänka på när man läser filer i C är att kontrollera om filen faktiskt kunde öppnas och läsas genom att kolla värdet som returneras av `fopen()`. Om filen inte finns eller om det finns några problem med att läsa den, kommer `fopen()` att returnera `NULL`.

## Se även

- [fopen() dokumentation](https://www.gnu.org/software/libc/manual/html_node/File-Open-Example.html#File-Open-Example)
- [fscanf() dokumentation](https://www.gnu.org/software/libc/manual/html_node/Formatted-Input.html#Formatted-Input)
- [En guide till C-filer](https://www.cprogramming.com/tutorial/cfileio.html)