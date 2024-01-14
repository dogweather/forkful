---
title:    "Haskell: Konvertere en dato til en tekststreng"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er en utbredt oppgave i programmering som kan være nyttig for å presentere informasjon til brukere på en mer lesbar måte. Ved å lære hvordan man utfører denne konverteringen i Haskell, kan du legge til en ekstra funksjonalitet til dine programmer.

## Hvordan

For å konvertere datoer til strenger i Haskell, kan du bruke funksjonen `formatTime` fra `Data.Time`. Her er et eksempel på hvordan du kan bruke denne funksjonen:

 ```Haskell
import Data.Time (UTCTime, defaultTimeLocale, formatTime)

-- Oppretter en UTCTime-objekt med dagens dato og tidspunkt
now :: UTCTime
now = getCurrentTime

-- Konverterer UTCTime-objektet til en streng ved hjelp av formatTime-funksjonen
dateAsString :: String
dateAsString = formatTime defaultTimeLocale "%d.%m.%Y" now

-- Printer den konverterte datoen til konsollen
main :: IO ()
main = putStrLn dateAsString

-- Output: 14.04.2021
```

Som du kan se i eksempelet, bruker vi `%d.%m.%Y` som et formatargument for `formatTime`-funksjonen. Dette indikerer at vi ønsker datoen som en streng i formatet "dag.måned.år". Du kan eksperimentere med ulike formatargumenter for å få ønsket datoformat.

## Dypdykk

Det er viktig å være klar over at `formatTime`-funksjonen krever at du importerer både `Data.Time` og `Data.Time.Format` på forhånd. I tillegg må du også importere `Data.Time.Locale` for å definere en standard tidsinnstilling. Du kan finne mer detaljert informasjon om dette i Haskell-dokumentasjonen.

## Se også

- [Haskell Dokumentasjon om Datoer](https://wiki.haskell.org/Date_and_time)
- [Brukerdefinerte tidsinnstillinger i Haskell](https://www.schoolofhaskell.com/user/pharpend/using-time)
- [Data.Time Hackage-dokumentasjon](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)