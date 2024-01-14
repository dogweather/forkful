---
title:    "Gleam: Konvertere en streng til små bokstaver"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver er en vanlig oppgave i programmering, spesielt når man jobber med tekstbehandling. Det kan for eksempel være nyttig når man ønsker å sammenligne to strenger, da man vil ha en standardisert form å sammenligne de på. Gleam gjør dette enkelt og elegant med sine innebygde funksjoner.

## Slik Gjør Du Det

For å konvertere en streng til små bokstaver i Gleam, bruker vi funksjonen `String.to_lower()`:

```Gleam
let ord = "PROGRAMMERING"
let konvertert_ord = String.to_lower(ord)
```

Her vil `konvertert_ord` inneholde strengen "programmering" i små bokstaver. Det er viktig å merke seg at denne funksjonen tar hensyn til språk og land, slik at den vil fungere korrekt for ulike språk og bokstavsett.

## Dykk Dypere

I Gleam, som i de fleste programmeringsspråk, er bokstaver og tegn representert ved hjelp av numeriske verdier. A-Å har for eksempel verdiene 65-90 for store bokstaver, og 97-122 for små bokstaver i ASCII-kodingen. Når vi konverterer en streng til små bokstaver, endrer vi dermed de numeriske verdiene til de tilsvarende små bokstavene.

Det er også verdt å nevne at Gleam har en egen funksjon, `String.to_ascii_lower()`, som kun tar hensyn til ASCII-kodingen og derfor er noe raskere enn `String.to_lower()`. Men for de fleste formål, er `String.to_lower()` det beste alternativet da det tar hensyn til ulike språk og bokstavsett.

## Se Også

- [Gleam dokumentasjon om strings](https://gleam.run/documentation/std_lib/#string)
- [ASCII-koding](https://no.wikipedia.org/wiki/ASCII)