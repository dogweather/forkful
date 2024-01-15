---
title:                "Å få nåtidig dato"
html_title:           "Haskell: Å få nåtidig dato"
simple_title:         "Å få nåtidig dato"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan du kan få tak i dagens dato i Haskell? Det kan være nyttig når du vil lage programmer som er avhengige av dagens dato, som for eksempel en kalenderapplikasjon eller et system for å føre oversikt over gjøremål. I denne artikkelen vil vi gå gjennom hvordan du kan få tak i dagens dato i Haskell på en enkel måte.

## Slik gjør du det

Først må du importere riktig modul for å få tak i dagens dato:

```Haskell
import Data.Time.Clock (UTCTime, getCurrentTime)
```

Deretter kan du bruke funksjonen `getCurrentTime` for å få tak i dagens dato og klokkeslett:

```Haskell
dato <- getCurrentTime
```

Denne funksjonen returnerer et `UTCTime`-objekt som inneholder informasjon om dagens dato og klokkeslett. Hvis du vil ha en mer leselig dato, kan du bruke funksjonen `utctDay` for å få tak i bare datoen:

```Haskell
dato <- getCurrentTime
let dag = utctDay dato
```

Dette vil returnere datoen i formatet `YYYY-MM-DD`. Hvis du heller vil ha datoen som en streng, kan du bruke funksjonen `show`:

```Haskell
dato <- getCurrentTime
let dag = show (utctDay dato)
```

Dette vil returnere datoen som en streng i formatet `YYYY-MM-DD`.

## Dypdykk

For å bruke funksjonen `getCurrentTime` må du være oppmerksom på at den returnerer et `IO UTCTime`-objekt. Dette betyr at den er innkapslet i en IO-monad, som er en måte Haskell håndterer input og output på. Dette er viktig å huske på når du skal bruke verdien fra funksjonen i resten av koden din.

Det er også verdt å merke seg at funksjonen `getCurrentTime` henter datoen fra systemklokken på datamaskinen din. Dette betyr at datoen kan være forskjellig hvis du kjører koden din på en annen datamaskin med en annen systemklokke.

## Se også

- [Offisiell dokumentasjon for Data.Time.Clock-modulen](https://hackage.haskell.org/package/time/docs/Data-Time-Clock.html)
- [En tutorial om monader i Haskell](https://en.wikibooks.org/wiki/Haskell/Understanding_monads)