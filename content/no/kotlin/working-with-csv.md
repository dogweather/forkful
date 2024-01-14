---
title:                "Kotlin: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

CSV-filer er et vanlig format for å lagre og dele data i ulike programmer. Å jobbe med CSV-filer kan være nyttig for både programmerere og ikke-programmerere, da det gir en enkel måte å organisere og analysere data på. I denne bloggposten vil vi ta en titt på hvordan vi kan jobbe med CSV-filer ved hjelp av Kotlin-programmeringsspråket.

## Slik gjør du det

Først og fremst må du importere biblioteket "kotlinx-csv" for å kunne jobbe med CSV-filer i Kotlin. Deretter kan du lese inn en CSV-fil ved å bruke funksjonen `CsvReader().open()` og spesifisere filstien til CSV-filen. For å få tilgang til dataene i CSV-filen, kan du bruke en løkke for å lese gjennom hver rad og deretter hente ut ønskede verdier ved å bruke indeksering.

La oss si at vi har en CSV-fil med følgende innhold:

```
Navn,Alder,By
Marius,28,Oslo
Sofie,25,Bergen
```

Vi kan lese inn denne filen og skrive ut alle navnene ved å bruke følgende kode:

```
val filsti = "/bruker/dokumenter/personer.csv"
val innhold = CsvReader().open(filsti)
innhold.forEach {
    println(it[0]) // skriver ut navnene
}
```

Du kan også skrive ut hele raden ved å bruke `println(it)`.

## Dypdykk

Det finnes mer avanserte metoder for å jobbe med CSV-filer i Kotlin. Du kan for eksempel bruke `CsvWriter` for å skrive data til en CSV-fil, og sjekke om en fil eksisterer før du prøver å lese den inn. Du kan også spesifisere separator og linjeskift når du leser inn og skriver til CSV-filer.

Kotlin har også innebygde funksjoner for å sortere og filtrere data fra CSV-filer, og du kan til og med bruke lambda-uttrykk for å gjøre dette på en enkel måte.

Det er viktig å huske på at det finnes ulike formater og standarder for CSV-filer, så det kan være lurt å dobbeltsjekke at filen du jobber med har riktig struktur og formatering.

## Se også

Her er noen nyttige ressurser for å jobbe med CSV-filer i Kotlin:

- [kotlinx-csv biblioteket](https://github.com/Kotlin/kotlinx-csv)
- [Kotlin CSV-håndbok](https://kotlinlang.org/docs/reference/idioms.html#read-csv-file-into-list-of-rows)
- [UOFFISIELL CSV-standard](https://tools.ietf.org/html/rfc4180)