---
title:                "C: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV-filer, eller "Comma Separated Values", är en vanlig filformat för att lagra och dela data. De är speciellt användbara för att lagra stora mängder data, som kan läsas och redigeras av både människor och datorer. Därför kan det vara fördelaktigt att lära sig hur man arbetar med CSV-filer i sitt programmeringsarbete.

## Hur du gör

För att arbeta med CSV-filer i C, behöver du först inkludera "stdio.h" i din kod, för att använda funktioner som t.ex. "fopen" och "fprintf". Sedan behöver du öppna CSV-filen med hjälp av "fopen" funktionen och använda en loop för att läsa data från filen rad för rad. Här är ett exempel på kod som läser data från en CSV-fil:

```
#include <stdio.h>

int main() {
    FILE *fp = fopen("data.csv", "r");

    if (fp == NULL) {
        printf("Kunde inte öppna filen.\n");
        return 1;
    }

    char line[100];

    while (fgets(line, 100, fp) != NULL) {
        printf("%s\n", line);
    }

    fclose(fp);

    return 0;
}
```

Om vi antar att "data.csv" innehåller följande data:

```
namn,ålder,ort
Erik,30,Stockholm
Maria,25,Göteborg
```

Så kommer output att bli:

```
namn,ålder,ort
Erik,30,Stockholm
Maria,25,Göteborg
```

## Djupdykning

Det finns många olika sätt att arbeta med CSV-filer i C, och en av de vanligaste är att använda "strtok" funktionen för att dela upp varje rad i olika fält baserat på separerings-tecknet (vanligtvis ett kommatecken). Det är också viktigt att hantera eventuella specialtecken som kan förekomma i data, som t.ex. citationstecken eller mellanslag.

En annan användbar funktion är "sscanf", som kan användas för att omvandla data från en sträng till olika variabler med hjälp av formateringssträngar.

Det finns också många bibliotek som kan hjälpa till att hantera CSV-filer i C, som t.ex. "libcsv" och "libcvsimple".

## Se även

- [libcsv documentation](https://github.com/embeddedartistry/libcsv)
- [libcvsimple documentation](https://github.com/gregor-aeschbacher/ttocsv)