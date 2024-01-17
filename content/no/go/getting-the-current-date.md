---
title:                "Å få gjeldende dato"
html_title:           "Go: Å få gjeldende dato"
simple_title:         "Å få gjeldende dato"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Få den nåværende datoen er en vanlig oppgave innen programmering. Datoen brukes til å holde oversikt over tidsrelaterte operasjoner og funksjoner. Dette kan være alt fra å vise dagens dato til å logge tidspunkter for bestemte hendelser.

## Hvordan:

Bruk `time.Now()` funksjonen for å få den nåværende datoen og klokkeslettet i Go. Se eksempelet nedenfor for implementasjon:

```Go
now := time.Now()
fmt.Println(now)
```
Output:
```
2020-09-23 18:30:45.123456789 +0800 CST m=+0.000011196
```

## Deep Dive:

Funksjonen `time.Now()` ble introdusert i Go versjon 1 tilbake i 2009. Den returnerer et `Time` objekt som inneholder all informasjon om den nåværende datoen og klokkeslettet. Alternativt kan du også bruke `time.Date()` for å spesifisere en spesifikk dato og klokkeslett. For mer detaljert informasjon om implementasjonen, sjekk ut Go sin offisielle dokumentasjon.

## Se Også:

For alternative måter å håndtere tid i Go, sjekk ut pakkene `time` og `date`. Du kan også sjekke ut Go sin standard bibliotek for flere nyttige funksjoner for håndtering av datoer og klokkeslett i dine programmer.