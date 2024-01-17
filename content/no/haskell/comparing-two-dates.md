---
title:                "Sammenligning av to datoer"
html_title:           "Haskell: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenligning av to datoer er en vanlig oppgave for programmere. Det innebærer å sjekke om en dato er før, etter eller lik en annen dato. Dette er nyttig for å organisere og filtrere data basert på datoer, for eksempel når man lager en kalender eller sorterer data basert på når de ble opprettet.

## Hvordan gjøre det:

```Haskell
-- Importer Data.Time biblioteket for å bruke funksjoner for datoer
import Data.Time

-- Opprett en utdatafunksjon for å vise sammenligningsresultat
output :: Ordering -> String
output LT = "Før"
output GT = "Etter"
output EQ = "Lik"

-- Definer to datoer (i dette tilfellet, 1. januar 2020 og 15. februar 2020)
d1 :: Day
d1 = fromGregorian 2020 1 1
d2 :: Day
d2 = fromGregorian 2020 2 15

-- Sammenlign datoene ved hjelp av compare-funksjonen
-- og skriv ut resultatet
main :: IO ()
main = do
    let comparison = compare d1 d2
    putStrLn $ "Dato 1 er " ++ output comparison ++ " dato 2."

-- Output:
-- Dato 1 er Før dato 2.
```

## Dykk dypere:

Sammenligning av datoer har alltid vært en viktig del av programmering, særlig med utviklingen av datamaskiner og datasystemer. Etter hvert som datamaskiner ble mer vanlige, ble behovet for å håndtere datoer og tidsdata mer komplekst. Dette førte til utviklingen av spesifikke funksjoner og biblioteker som Data.Time i Haskell, som gjør det enkelt å håndtere og sammenligne datoer.

Det finnes også alternative måter å sammenligne datoer på, for eksempel ved å representere dem som tall og bruke vanlige matematiske operatorer. Dette kan være en enklere tilnærming, men det kan også føre til feil og unøyaktigheter når man håndterer tidsdata.

Når det kommer til implementasjonsdetaljer, bruker Data.Time-biblioteket i Haskell konseptet om "Julian Day Numbers" for å representere datoer. Dette er et numerisk system som angir antall dager siden en bestemt startdato. Ut fra dette kan man enkelt bruke vanlige numeriske operatorer for å sammenligne datoer.

## Se også:

- [Haskell wiki - Data.Time](https://wiki.haskell.org/Data.Time)
- [Haskell.org - Data.Time dokumentasjon](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/time-1.9.3/Data-Time.html)
- [W3 Schools - Sammenligning av datoer i Haskell](https://www.w3schools.com/haskell/haskell_date_comparison.asp)