---
title:                "Haskell: Å få gjeldende dato"
simple_title:         "Å få gjeldende dato"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Har du noen gang ønsket å få den nåværende datoen i programmene dine? Kanskje du trenger å lagre datoen for en hendelse eller bare vil vise den for brukeren din. Uansett årsaken, er det viktig å kunne hente dagens dato i Haskell.

## Hvordan
Først må vi importere `Data.Time` biblioteket ved å skrive følgende linje øverst i filen vår:

```Haskell
import Data.Time
```

Vi kan deretter bruke `getCurrentTime` -funksjonen for å få den nåværende datoen og klokkeslettet som en `UTCTime` -verdi. Dette kan gjøres ved å kalle funksjonen i en `IO` -handling og bruke `print` for å skrive ut resultatet:

```Haskell
getCurrentTime >>= print
```

Dette vil skrive ut noe lignende dette:

```
2021-10-19 13:00:00.000000000 UTC
```

For å få en mer lesbar formatert dato, kan vi bruke funksjonen `formatTime` sammen med en `TimeLocale` for å angi format og språk. For eksempel, for å få datoen i formatet "dd.mm.yyyy", kan vi skrive:

```Haskell
getCurrentTime >>= print . formatTime defaultTimeLocale "%d.%m.%Y"
```

Dette vil skrive ut noe lignende dette:

```
19.10.2021
```

## Dypdykk
Haskell har en rekke nyttige funksjoner for å jobbe med datoer og klokkeslett. For å få mer detaljert informasjon om en `UTCTime` -verdi, kan vi bruke funksjonen `getUTCTimeComponents` for å få en strukturert verdi som inneholder år, måned, dag, time, minutt, sekund og millisekund.

Videre kan vi bruke funksjonen `addUTCTime` for å legge til et bestemt antall sekunder til en `UTCTime` -verdi. Dette kan være nyttig for å beregne fremtidige eller tidligere datoer og klokkeslett.

## Se også
- [Offisiell dokumentasjon for Data.Time-biblioteket](https://hackage.haskell.org/package/time-1.9/docs/Data-Time.html)
- [Haskell Wiki: Clocks og Time](https://wiki.haskell.org/Clocks_and_time)