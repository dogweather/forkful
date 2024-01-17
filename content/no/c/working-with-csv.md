---
title:                "Arbeid med CSV"
html_title:           "C: Arbeid med CSV"
simple_title:         "Arbeid med CSV"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-csv.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

CSV, eller Comma-Separated Values, er en filformat som brukes til å lagre enkle tabellstrukturer i tekstformat. Mange programmerere jobber med CSV-filer fordi de er en praktisk og vanligvis lett måte å lagre og behandle data på. Dette er spesielt nyttig når man trenger å overføre data mellom forskjellige programmer og systemer.

# Hvordan?

CSV-filer består av tekstlinjer som er delt inn i kolonner ved hjelp av komma, og vanligvis bruker de filendelsen .csv. Her er et eksempel på hvordan CSV-data kan se ut:

```C
Navn, Alder, Jobb
Johan, 25, Programmerer
Maria, 32, Webdesigner
Erik, 45, Markedsfører
```

For å lese og behandle data fra en CSV-fil i C trenger man et bibliotek, for eksempel libcsv. Ved å bruke dette biblioteket kan man åpne og lese en CSV-fil, og behandle dataene som er lagret i filen. Her er et eksempel på hvordan man kan lese en CSV-fil med libcsv:

```C
#include <csv.h>

int main() {
    // Åpner en CSV-fil for lesing
    FILE* fil = fopen("data.csv", "r");
    
    // Lager en CSV-parser for å lese dataene fra filen
    csv_parser_t* parser = csv_parser_new();
    
    // Leser CSV-dataene fra filen og lagrer dem i variabler
    char* navn;
    int alder;
    char* jobb;
    while (csv_parse_next_row(parser)) {
        csv_parse_string(parser, 0, &navn, NULL);
        csv_parse_int(parser, 1, &alder, NULL);
        csv_parse_string(parser, 2, &jobb, NULL);
        
        // Gjør noe med dataene, f.eks. skriv til konsollen
        printf("%s er %d år og jobber som %s\n", navn, alder, jobb);
    }
    
    // Frigjør ressurser
    fclose(fil);
    csv_parser_destroy(parser);
}
```

Eksempelutdata:

```
Johan er 25 år og jobber som Programmerer
Maria er 32 år og jobber som Webdesigner
Erik er 45 år og jobber som Markedsfører
```

# Dypdykk

CSV ble opprinnelig utviklet for å gjøre det enklere å importere og eksportere data mellom regnearkprogrammer og databasesystemer på 80-tallet. Det har etter hvert blitt et populært format for å lagre og behandle data på grunn av sin enkelhet og kompatibilitet med forskjellige programmer og systemer.

Det finnes ulike måter å håndtere CSV-data på i C, som for eksempel å bruke standardbiblioteket `<stdio.h>` eller spesifikke CSV-biblioteker som libcsv. Det er viktig å være oppmerksom på at data fra CSV-filer ofte trenger en viss form for validering før de kan behandles, særlig hvis de skal brukes i kritiske systemer.

# Se også

- [RFC 4180: Common format and MIME type for CSV files](https://tools.ietf.org/html/rfc4180)
- [libcsv](https://github.com/akheron/libcsv) – et populært CSV-bibliotek for C
- [Using CSV Files in C/C++](https://www.geeksforgeeks.org/csv-file-handling-c/), en guide for å lese og behandle CSV-filer i C