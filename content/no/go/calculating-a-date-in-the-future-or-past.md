---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Go: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
 
Beregning av en dato i fremtiden eller fortiden handler om å få dataprogrammer til å utføre matematiske operasjoner på datoer. Dette er nyttig fordi det gjør det enklere å håndtere tidsavhengige oppgaver, som for eksempel å beregne forfallsdatoer for betalinger eller å planlegge fremtidige hendelser.

## Slik gjør du det:

Først må du importere tidspakken i Go ved å skrive `import "time"` øverst i koden din. Deretter kan du bruke funksjonen `AddDate()` for å legge til eller trekke fra et gitt antall år, måneder og dager fra en dato. For eksempel, for å beregne datoen 365 dager fra i dag, kan du bruke følgende kode:

```
Go package main

import (
    "fmt"
    "time"
)

func main() {
    d := time.Now() 
    d = d.AddDate(0, 0, 365) 
    fmt.Println(d) 
}
```

Dette vil gi følgende utgang: `2022-05-07 11:45:42.349698613 +0200 CEST`.

## Dypdykk:

Beregning av datoer i fremtiden eller fortiden er ikke en unik funksjon i Go, og kan også implementeres i andre programmeringsspråk som JavaScript og Python. I tidligere versjoner av Go ble dette gjort ved hjelp av funksjoner som `Date()` og `Time()`, men disse er nå avskaffet til fordel for `time.Now()` og `AddDate()`.

Vær også oppmerksom på at beregning av datoer i fremtiden eller fortiden kan være komplisert på grunn av skuddårsreglene og ulike datostandarder rundt om i verden. Det anbefales å bruke Go's innebygde tidsfunksjoner for å være sikker på at resultatet er korrekt.

## Se også:

- [Go's offisielle dokumentasjon for tidspakken](https://golang.org/pkg/time/)
- [Hvordan beregne datoer i fremtiden og fortiden i JavaScript](https://www.w3schools.com/js/js_date_methods.asp)
- [Generell informasjon om dato- og tidsberegning](https://www.timeanddate.com/date/duration.html)