---
title:                "Werken met CSV"
date:                  2024-01-28T22:10:34.439245-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met CSV"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met CSV (Comma-Separated Values) betekent het verwerken van gegevens gestructureerd als platte tekst waarbij elke regel velden heeft gescheiden door komma's. Programmeurs gebruiken CSV omdat het eenvoudig is, breed ondersteund wordt, en makkelijk integreert met spreadsheets en databases.

## Hoe:

Laten we een CSV-bestand parsen met basis C-code. We lezen een bestand, splitsen elke regel in velden, en drukken deze af.

```C
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Kan bestand niet openen\n");
        return 1;
    }

    char line[256];
    while (fgets(line, sizeof(line), fp)) {
        char *token = strtok(line, ",");
        while (token) {
            printf("%s\n", token);
            token = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```

Voorbeeld `data.csv`:
```
name,age,city
Alice,30,New York
Bob,25,Los Angeles
```

Voorbeelduitvoer:
```
name
age
city
Alice
30
New York
Bob
25
Los Angeles
```

## Diepgaand Onderzoek

CSV-bestanden worden al sinds de vroege dagen van personal computing gebruikt vanwege hun eenvoud. Alternatieven zoals JSON of XML brengen meer complexiteit met zich mee, maar bieden gestructureerde gegevensrepresentatie. Bij het implementeren van CSV-parsing moet zorgvuldig worden omgegaan met randgevallen zoals velden die komma's of nieuwe regels bevatten, welke volgens de CSV-standaard (RFC 4180) tussen aanhalingstekens moeten staan.

## Zie Ook

- [RFC 4180](https://tools.ietf.org/html/rfc4180): Het gemeenschappelijke formaat en MIME-type voor Comma-Separated Values (CSV)-bestanden.
- [libcsv](http://sourceforge.net/projects/libcsv/): Een C-bibliotheek voor CSV-parsing.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/csv?tab=Votes): Gemeenschapsdiscussies en V/A over CSV-gerelateerde programmeerproblemen.
