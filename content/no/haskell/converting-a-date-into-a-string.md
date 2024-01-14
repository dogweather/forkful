---
title:                "Haskell: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Har du noen gang hatt behov for å konvertere en dato til en streng? Det kan være en nødvendig oppgave når du jobber med datoer i programmering, og i denne artikkelen skal vi gå gjennom hvordan du kan gjøre det på en enkel måte med Haskell.

## Hvordan
For å konvertere en dato til en streng i Haskell, kan vi bruke funksjonen `show` og `toGregorian` fra standardbiblioteket `Data.Time`. Først må vi importere disse modulene:

```Haskell
import Data.Time
import Data.Time.Format
```

Deretter kan vi definere en funksjon som tar inn en `Day`-verdi (en datatype som representerer en dato i Haskell) og returnerer en streng med formatet "dd/mm/yyyy":

```Haskell
-- Funksjon for å konvertere dato til streng
datoTilStreng :: Day -> String
-- Bruker show for å konvertere Day-verdien til en streng, 
-- og formatTime for å spesifisere ønsket format
datoTilStreng dato = formatTime defaultTimeLocale "%d/%m/%Y" dato
```

Vi kan nå kjøre funksjonen med en `Day`-verdi som argument, for eksempel en dato fra i dag:

```Haskell
>>> datoTilStreng $ utskrift $ datoTilStreng dato
"03/07/2020"
```

Som du kan se, returnerer funksjonen en streng med dagens dato i ønsket format.

## Deep Dive
Det finnes flere funksjoner i `Data.Time`-modulen som kan være nyttige for å arbeide med datoer i Haskell. For eksempel kan vi bruke `getCurrentTime`-funksjonen for å få nåværende dato og klokkeslett som en `UTCTime`-verdi, og deretter konvertere denne til en lokal tidssone ved hjelp av `utcToLocalTime`-funksjonen.

Ønsker du å gjøre mer avanserte operasjoner med datoer, kan du sjekke ut modulen `Data.Time.Calendar` som inneholder funksjoner for å manipulere datoer og beregne forskjellen mellom to datoer.

## Se også
- [Haskell Docs: Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Learn You a Haskell: Time](http://learnyouahaskell.com/input-and-output#hello-world)