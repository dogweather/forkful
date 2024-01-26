---
title:                "Arbeid med CSV"
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Arbeid med CSV (Comma-Separated Values) handler om å lese, skrive, og manipulere data i tekstfiler hvor hver linje er en dataoppføring, og hver verdi er adskilt med komma. Programmerere bruker CSV fordi det er en enkel, lettleselig filformat som er kompatibel med de fleste tabellprogrammer og databaser.

## Hvordan gjøre det:
Her er hvordan du kan lese og skrive CSV-filer i C.

### Lesing av en CSV-fil:
```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LEN 1024

int main() {
    char buf[MAX_LEN];
    FILE *fp = fopen("data.csv", "r");

    if (!fp) {
        printf("Kan ikke åpne filen.\n");
        return 1;
    }

    while (fgets(buf, MAX_LEN, fp)) {
        char* val = strtok(buf, ",");
        while (val) {
            printf("%s\n", val);
            val = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```

### Skriving til en CSV-fil:
```C
#include <stdio.h>

int main() {
    FILE *fp = fopen("utdata.csv", "w");

    if (!fp) {
        printf("Kan ikke åpne filen.\n");
        return 1;
    }

    fprintf(fp, "%s,%s,%s\n", "Navn", "Alder", "By");
    fprintf(fp, "%s,%d,%s\n", "Olav", 25, "Oslo");
    fprintf(fp, "%s,%d,%s\n", "Kari", 30, "Bergen");

    fclose(fp);
    return 0;
}
```

Utdata for lesing vil være verdier fra `data.csv` filen listet i konsollen, og skriving vil skape `utdata.csv` med gitt informasjon.

## Dypdykk
CSV ble populært på 1970-tallet og er fortsatt i bruk for sin enkelhet. Alternativer til CSV inkluderer JSON, XML, og databaser som SQLite. Nøkkelen til effektiv håndtering av CSV i C er riktig bruk av `stdio.h` funksjoner og bufferhåndtering for å unngå overflow og minnelekkasjer. For større filer eller mer kompleks data, bør man vurdere en CSV parser bibliotek.

## Se også:
- [RFC 4180](https://tools.ietf.org/html/rfc4180), som gir grunnlaget for CSV-formatet.
- [libcsv](http://libcsv.sourceforge.net/), et bibliotek for å lese og skrive CSV-filer i C.
- [SQLite](https://www.sqlite.org/index.html), et enkelt databasesystem som kan brukes for lagring og spørringer av mer kompleks data.
