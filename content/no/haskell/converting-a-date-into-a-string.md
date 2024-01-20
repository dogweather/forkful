---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en dato til en streng i programmering refererer til prosessen med å endre datoen fra dens standard format til en tekstrepresentasjon. Programmerere gjør dette for å gjøre data mer menneskelig lesbar og for å lette datautveksling mellom forskjellige softwarekomponenter som forbruker eller produserer datoer i ulike formater.

## Hvordan gjøre det:
Nedenfor er en enkel Haskell funksjon som konverterer en dato til en streng:

```Haskell
import Data.Time.Clock
import Data.Time.Format
import System.Locale

dateToString :: UTCTime -> String
dateToString date = formatTime defaultTimeLocale "%d.%m.%Y" date
```
La oss prøve denne koden:

```Haskell
main :: IO ()
main = do
    currentTime <- getCurrentTime
    putStrLn $ dateToString currentTime
```

Når du kjører dette programmet, skulle utdataene være dagens dato, formater som "dd.mm.yyyy".

## Dypdykk
Historisk sett hadde ikke Haskell innebygd dato-til-streng-funksjonalitet. Dette endret seg med introduksjonen av `Data.Time`-biblioteket, som gjør det mye enklere å håndtere datoer og tider. 

Som et alternativ kan du bruke eldre `Time`- biblioteket, men det anbefales ikke på grunn av mindre intuitiv API og begrensede funksjoner.

Haskell bruker ovenstående formatfunksjon fra `Data.Time.Format` for å konvertere datoer til strenger. Denne funksjonen tar i bruk mønstermatchingsprinsippet til å mappe datoobjekter til strengrepresentasjoner basert på formatet du angir. 

## Se Også
Du kan lese mer om dato og tid i Haskell på de følgende lenkene:

- `Data.Time` dokumentasjon: [https://hackage.haskell.org/package/time-1.8.0.2/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.8.0.2/docs/Data-Time.html)

- `Data.Time.Format` dokumentasjon: [https://hackage.haskell.org/package/time-1.8.0.2/docs/Data-Time-Format.html](https://hackage.haskell.org/package/time-1.8.0.2/docs/Data-Time-Format.html).

- Haskell Wiki om tid: [https://wiki.haskell.org/Time](https://wiki.haskell.org/Time).