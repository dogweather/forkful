---
title:                "Gleam: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er en vanlig oppgave i programmering, spesielt når man jobber med brukerinput og databaser. I Gleam kan dette gjøres enkelt og effektivt ved å følge noen få trinn.

## Slik gjør du det

For å konvertere en dato til en streng i Gleam, kan du følge følgende kodeeksempel:

```Gleam
import gleam/zx
import time

let now = time.now()
let formatted = time.format(now, "%d.%m.%Y")
zx.println(formatted)
```

Dette eksempelet viser hvordan du bruker Gleams innebygde bibliotek for tid og formaterer datoen ved hjelp av spesifikke formateringsstrenger. Output blir da følgende:

```
17.03.2021
```

I dette eksempelet brukte vi "%d.%m.%Y" for å angi at datoen skal vises med dag, måned og år på formatet DD.MM.YYYY.

## Dykk dypere

Å konvertere datoer til strenger kan være nyttig når man skal utføre ulike operasjoner med datoen, som å vise den på en bestemt måte eller lagre den i en database. I Gleam kan man også benytte seg av biblioteker som "chronos" og "dateformat" for mer avanserte operasjoner med datoer og tidsformatering.

## Se også

- Gleams offisielle dokumentasjon for tid og dato: https://gleam.run/documentation/stdlib/time
- "Chronos" biblioteket: https://github.com/awetzel/chronos
- "Dateformat" biblioteket: https://github.com/tmattio/dateformat