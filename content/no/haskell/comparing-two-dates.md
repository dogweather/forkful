---
title:    "Haskell: Sammenligning av to datoer"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer er en vanlig oppgave i programmering, spesielt når man jobber med tidssensitive data eller funksjonalitet. Ved å lære hvordan man sammenligner to datoer i Haskell, kan du enkelt håndtere og manipulere datoer i dine programmer.

## Hvordan gjøre det

For å sammenligne to datoer i Haskell, kan du bruke funksjonen `compare`. Denne funksjonen tar inn to datotyper, og returnerer en `Ordering` type som enten kan være `LT` (mindre enn), `GT` (større enn), eller `EQ` (lik). Her er et eksempel som sammenligner to datoer og skriver ut resultatet:

```Haskell
import Data.Time.Calendar

today = fromGregorian 2020 11 01 -- dd.mm.åååå format
tomorrow = fromGregorian 2020 11 02

main = print $ compare today tomorrow
```

Dette vil gi følgende output:

```
LT
```

Dette betyr at `today` er mindre enn `tomorrow`. Hvis de to datoene hadde vært like, ville output vært `EQ`. Du kan også bruke funksjonen `diffDays` for å beregne antall dager mellom to datoer:

```Haskell
import Data.Time.Calendar

today = fromGregorian 2020 11 01
yesterday = fromGregorian 2020 10 31

main = print $ diffDays today yesterday
```

Dette vil gi følgende output:

```
1
```

Som du ser, er det viktig å huske på datoformatet når du sammenligner eller manipulerer datoer.

## Dykk dypere

Haskell har en egen modul for å håndtere datoer og tid, `Data.Time.Calendar`. Denne modulen tilbyr en rekke funksjoner og datatype for å arbeide med datoer og tid. Du kan lese mer om denne modulen i Haskell sin offisielle dokumentasjon.

## Se også

- [Haskell sin offisielle dokumentasjon](https://www.haskell.org/documentation/)
- [Data.Time.Calendar modulen](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html)