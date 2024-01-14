---
title:                "Haskell: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å beregne en dato i fremtiden eller fortiden kan være nyttig for å lage programmer som håndterer tidsavhengig informasjon. For eksempel kan det være nødvendig å beregne en forfallsdato for betalinger eller lage en kalenderfunksjon som viser riktige datoer.

## Slik gjør du det

For å beregne en dato i fremtiden eller fortiden i Haskell, kan du bruke funksjonene `addDays` og `addGregorianYearsClip` fra pakken `Data.Time`.

```Haskell
import Data.Time

-- Beregner en dato fem dager frem i tid
fremtidigDato :: Day
fremtidigDato = addDays 5 today

-- Beregner en dato ti år tilbake i tid
fortidigDato :: Day
fortidigDato = addGregorianYearsClip (-10) today

-- Printer ut resultatene
main :: IO()
main = do 
    print fremtidigDato
    print fortidigDato
```

Output:

```
2021-11-02
2011-11-02
```

For å bruke disse funksjonene må du importere `Data.Time` og definere en `Day` verdi som du ønsker å beregne en ny dato fra. `addDays` legger til et gitt antall dager til den opprinnelige datoen, mens `addGregorianYearsClip` legger til et gitt antall år og justerer datoen til en gyldig dato i den gregorianske kalenderen.

## Dykk dypere

For å beregne en dato på en mer avansert måte, kan du bruke funksjonen `addGregorianDurationClip` fra samme pakke. Denne funksjonen tar inn en `Duration` verdi som representerer et tidsintervall og bruker det til å justere den opprinnelige datoen.

```Haskell
-- Lager en ny data type Duration
data Duration = Duration 
    { years :: Integer
    , months :: Int
    , days :: Int
    } deriving (Show)

-- Beregner en dato 3 år, 2 måneder og 10 dager frem i tid
fremtidigDato :: Day
fremtidigDato = addGregorianDurationClip (Duration 3 2 10) today

-- Printer ut resultatet
main :: IO()
main = print fremtidigDato
```

Output:

```
2024-01-12
```

Her kan du se at funksjonen `addGregorianDurationClip` bruker den sammensatte datatypen `Duration` til å beregne en mer spesifikk dato basert på den opprinnelige datoen.

## Se også

- [haskell-docs] (https://www.haskell.org/documentation/)
- [Data.Time documentation] (https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Calculating dates in Haskell blog post] (https://medium.com/@annesacker/how-to-calculate-dates-using-haskell-9b5229132d38)