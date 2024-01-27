---
title:                "Arbeta med csv"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? ("Vad & Varför?")
Arbete med CSV innebär att hantera data i "Comma-Separated Values" formatet, vilket är vanligt för datadelning. Programmerare gör detta för att enkelt utbyta och manipulera data mellan olika program och system.

## How to: ("Hur man gör:")
```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *fp = fopen("exempel.csv", "r");
    if (!fp) {
        printf("Kunde inte öppna filen.\n");
        return EXIT_FAILURE;
    }

    char buf[1024];
    int row_count = 0;
    int field_count = 0;
    
    while (fgets(buf, 1024, fp)) {
        field_count = 0;
        row_count++;
        
        if (row_count == 1) continue; // Skippar rubrik
        
        char *field = strtok(buf, ",");
        while (field) {
            printf("%s ", field);
            field = strtok(NULL, ",");
            field_count++;
        }
        printf("\n");
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
```
Sample output for a CSV containing names and ages:
```
Alice 23 
Bob 34 
Charlie 29 
```

## Deep Dive ("Djupdykning")
CSV-formatets enkelhet härstammar från tidiga dataprogram. Alternativ som JSON eller XML erbjuder rikare datatyper och strukturer men är mer komplexa. När du hanterar CSV-filer i C måste du ofta skriva egen kod för att parsa och validera data, vilket innebär att du måste hantera potentiella fallgropar som specialtecken och inkonsekvent användning av kommatecken.

## See Also ("Se även"):
- [RFC 4180, Common Format and MIME Type for Comma-Separated Values (CSV) Files](https://tools.ietf.org/html/rfc4180)
- [libcsv](http://sourceforge.net/projects/libcsv/), ett bibliotek för att hantera CSV i C
- [GNU Datamash](https://www.gnu.org/software/datamash/), ett kommandoradsverktyg som kan utföra enkla manipulationer på textfiler, inklusive CSV.
