---
title:    "Haskell: Å få den nåværende datoen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du driver med programmering, er det stor sjanse for at du vil trenge å få tak i den aktuelle datoen på et eller annet tidspunkt. Enten det er for å logge når en hendelse fant sted, eller for å velge ut bestemte data basert på dato. Heldigvis kan moderne programeringsspråk som Haskell gjøre dette prosessen enkel og effektiv.

## Hvordan gjøre det

Først må du importere det innebygde datatypen `Data.Time` ved hjelp av følgende kommando:

```Haskell
import Data.Time
```

Deretter kan du bruke funksjonen `getCurrentTime` for å få tak i dagens dato og klokkeslett:

```Haskell
dagensDato <- getCurrentTime
```

Du kan også formatere dato og klokkeslett på forskjellige måter ved å bruke funksjonen `formatTime`:

```Haskell
formatTime defaultTimeLocale "%A, %e %B %Y" dagensDato
```

Dette vil gi ut en tekststreng som viser dagens dato i formatet "dag, dd. måned yyyy". Du kan også legge til klokkeslett ved å bruke et annet parameter i funksjonen `formatTime`:

```Haskell
formatTime defaultTimeLocale "%H:%M" dagensDato
```

Resultatet blir da en tekststreng som viser klokkeslettet i formatet "tt:mm".

## Dypdykk

Haskell har også flere moduler som kan hjelpe deg med å håndtere datoer og klokkeslett mer presist. For eksempel, hvis du jobber med internasjonalisering, kan du bruke `Data.Time.Locale` for å få lokal oversettelse av måneder og ukedager. Hvis du trenger å håndtere tidssoner, kan du bruke modulen `Data.Time.LocalTime` som lar deg konvertere mellom forskjellige tidssoner.

En annen nyttig funksjon er `parseTimeM`, som lar deg konvertere tekststrenger til dato- og klokkeslettsverdier. Dette kan være spesielt nyttig hvis du leser inn data fra en fil eller en brukerinput.

## Se også

- [Haskell-dokumentasjonen for Data.Time](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/time-1.9.3/Data-Time.html)
- [Tutorial om Data.Time på Haskell-programmetringsbloggen](https://www.haskell.org/tutorial/numbers.html#date-time)
- [Haskell Wiki om dato og klokkeslett](https://wiki.haskell.org/Date_and_time_libraries)