---
title:    "Gleam: Sammenligning av to datoer"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor

Sammenligning av to datoer er en vanlig oppgave i mange programmeringsprosjekter. Det kan være nyttig for å filtrere data, beregne forskjeller i tid, eller for å organisere informasjon etter dato. I Gleam programmeringsspråket, kan du enkelt sammenligne to datoer ved å følge noen enkle trinn.

## Slik gjør du det

For å sammenligne to datoer i Gleam, må du først importere standardbiblioteket `gleam/datetime` og modulen `Datetime.Comparisons`. Deretter må du lage to `Datetime`-verdier med ønskede datoer. Du kan gjøre dette på flere måter, for eksempel ved å bruke funksjonene `Datetime.from_erlang` eller `Datetime.from_iso8601`.

```
import gleam/datetime
import gleam/datetime/comparisons as datetime

let date1 = Datetime.from_erlang({{2021, 6, 15}})
let date2 = Datetime.from_iso8601("2021-06-20")
```

Nå kan du bruke forskjellige funksjoner fra `Datetime.Comparisons`-modulen for å sammenligne de to datoene. For eksempel kan du bruke `Datetime.Comparisons.lt` for å sjekke om `date1` er før `date2`.

```
let is_before = datetime.lt(date1, date2)
```

Du kan også bruke `Datetime.Comparisons.compare` for å få en sammenligning mellom de to datoene i form av en `Ordering`-verdi.

```
let ordering = datetime.compare(date1, date2)
```

Output av disse funksjonene vil være en `Bool`-verdi og en `Ordering`-verdi, henholdsvis. Disse delene av koden kan også settes inn i en `match`-blokk for å håndtere forskjellige tilfeller av sammenligningen.

## Dykk dypere

For mer detaljert informasjon om sammenligning av datoer i Gleam, anbefales det å se nærmere på standardbibliotekets dokumentasjon. Der kan du finne en liste over alle tilgjengelige funksjoner for å sammenligne og manipulere datoer, samt eksempler og forklaringer på hvordan de brukes.

## Se også

- [Gleam standardbibliotek dokumentasjon](https://gleam.run/documentation/stdlib/#datetime)
- [Gleam Språk Referanse](https://gleam.run/documentation/language_reference/#comparisons)