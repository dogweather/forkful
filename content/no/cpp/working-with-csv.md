---
title:                "Arbeid med csv"
html_title:           "C++: Arbeid med csv"
simple_title:         "Arbeid med csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Programmering med CSV står for Comma-Separated Values og er en vanlig måte å organisere tabell-lignende data. Det er nyttig for programmerere fordi det gjør det lett å lagre og lese strukturerte data.

## Hvordan:
For å arbeide med CSV-strukturer i C++, må du bruke ferdige biblioteker. Et populært alternativ er "CSV-parser" som gjør det enkelt å lese og skrive CSV-filer. Her er et eksempel på hvordan man kan lese data fra en CSV-fil og skrive den ut til konsollen:

```C++
#include <iostream>
#include "csv.h"

int main() {
    io::CSVReader<2> in("data.csv"); // Angi filnavn
    in.read_header(io::ignore_extra_column, "Name", "Age"); // Angi kolonnenavn
    std::string name;
    int age;
    while (in.read_row(name, age)) { // Gå gjennom hver rad
        std::cout << name << " er " << age << " år gammel." << std::endl; // Skriv ut data
    }
    return 0;
}
```

Her er et eksempel på data som kan være lagret i "data.csv" filen:

```
Name, Age
John, 25
Sarah, 32
David, 19
```
Output: 
```
John er 25 år gammel.
Sarah er 32 år gammel.
David er 19 år gammel.
```

## Dypdykk:
CSV-formatet har vært i bruk siden 1970-tallet og har siden blitt en populær måte å lagre og overføre data på. Alternativene til CSV inkluderer XML og JSON, men CSV er fortsatt et godt valg for enkel og strukturert data.

Parsing av CSV-data kan være krevende på grunn av forskjeller mellom implementasjoner og varierte formater. Derfor er det ofte en god ide å bruke et pålitelig bibliotek som "CSV-parser" for å sikre riktig håndtering av data.

## Se også:
For mer informasjon om CSV-formatet og hvordan man kan bruke det i C++, sjekk ut disse kildene:

- [CSV format](https://en.wikipedia.org/wiki/Comma-separated_values)
- [CSV-parser på GitHub](https://github.com/vincentlaucsb/csv-parser)