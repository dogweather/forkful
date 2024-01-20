---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Sammenligne Datoer med Haskell

## Hva & Hvorfor?

Sammenligning av to datoer betyr å bestemme hvilken dato som kommer først eller om begge er de samme. Programmerere gjør dette for å håndtere tidslinjer, løse tidsavhengige problemer og utføre dato-aritmetikk.

## Hvordan:

Bruk `Data.Time` biblioteket for å sammenligne datoer i Haskell.

```Haskell
import Data.Time

main = do
    let dato1 = ModifiedJulianDay 59000
    let dato2 = ModifiedJulianDay 60200
    putStrLn $ if dato1 < dato2 then "Dato1 er tidligere." else "Dato2 er tidligere."
```

Hvis du kjører denne, vil output være:

```Haskell
"Dato1 er tidligere."
```

## Dypdykk

Sammenligning av datoer har vært en kritisk del av programmering siden dets tidlige dager. Haskell, opprinnelig født i 1990, har blitt brukt i bredt spekter av applikasjoner som inkluderer tid og dato manipulasjoner.

Alternativer til `Data.Time` inkluderer `Time` og `Data.Dates` bibliotekene. Men `Data.Time` er det mest anbefalte biblioteket for håndtering av tid og dato i Haskell fordi det er grundig og nøye testet.

Sammenligning av datoer i Haskell er en grei prosess fordi alle datoobjekter er av typen `Ord`, som betyr at de inneholder innebygde sammenligningsoperasjoner. Data typen `ModifiedJulianDay` vi brukte ovenfor er et eksempel.

## Se også 

For mer informasjon om dato- og tidsfunksjoner i Haskell, se følgende kilder:

1. [Data.Time](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
3. For et dypere dykk med tid og dato håndtering i Haskell, sjekk [School of Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks).