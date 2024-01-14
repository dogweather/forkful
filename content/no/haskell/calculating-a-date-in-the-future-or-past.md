---
title:    "Haskell: Å beregne en dato i fremtiden eller fortiden"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Å beregne en dato i fremtiden eller fortiden kan være nyttig for ulike programmeringsscenarier, som for eksempel planlegging av hendelser eller generering av rapporter basert på bestemte datoer.

## Hvordan

For å beregne en dato i Haskell, kan vi bruke `Data.Time` modulen som inneholder funksjoner og datatyper relatert til dato og tid.

Først importerer vi modulen ved å legge til følgende linje øverst i filen vår:

```Haskell
import Data.Time
```

For å beregne en dato i fremtiden, kan vi bruke `addDays` funksjonen og spesifisere antall dager vi ønsker å legge til. For eksempel, hvis vi ønsker å finne ut datoen 10 dager fra nå, kan vi skrive følgende:

```Haskell
futureDate = addDays 10 getCurrentTime
-- Legger til 10 dager til dagens dato
```

For å beregne en dato i fortiden, kan vi bruke `addDays` funksjonen igjen, men med et negativt antall dager. For eksempel, hvis vi ønsker å finne ut datoen 10 dager tilbake i tid, kan vi skrive følgende:

```Haskell
pastDate = addDays (-10) getCurrentTime
-- Trekker fra 10 dager fra dagens dato
```

## Dypdykk

I tillegg til å legge til og trekke fra dager, kan vi også bruke `Data.Time` modulen til å manipulere datoer på andre måter. For eksempel kan vi bruke `addGregorianMonthsClip` funksjonen til å legge til et gitt antall måneder til en dato. Denne funksjonen håndterer også upassende måneder som februar, og justerer datoen slik at den er gyldig.

Vi kan også bruke `diffDays` funksjonen til å finne antall dager mellom to datoer. Dette kan være nyttig for å beregne antall dager mellom to hendelser eller for å filtrere data basert på bestemte tidsintervaller.

## Se også

- [Offisiell Haskell dokumentasjon om `Data.Time` modulen](https://hackage.haskell.org/package/time-1.10.0.0/docs/Data-Time.html)
- [Flere eksempler på å beregne datoer i Haskell](https://www.schoolofhaskell.com/school/basic-haskell/dates-and-times)
- [En oversikt over funksjonene i `Data.Time` modulen](https://steveholgado.com/blog/2016/03/using-datetimes-in-haskell/)