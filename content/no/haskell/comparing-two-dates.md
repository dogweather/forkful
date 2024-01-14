---
title:    "Haskell: Sammenligne to datoer"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Det å sammenligne to datoer kan være nyttig når du jobber med data eller trenger å vise informasjon basert på ulike tidspunkter. Ved å sammenligne datoer i Haskell, kan du enkelt sortere og filtrere data eller utføre beregninger basert på når dataen ble samlet inn. Dette kan være spesielt nyttig i finans- eller statistikkrelaterte applikasjoner.

## Hvordan

For å sammenligne to datoer i Haskell, kan vi bruke funksjonen `diffDays` fra `Data.Time.Calendar` biblioteket. Denne funksjonen tar to `Day` objekter som argumenter og returnerer antall dager mellom disse to datoene. La oss se et eksempel på hvordan dette kan brukes:

```Haskell
import Data.Time.Calendar

-- Definerer to datoer
dato1 = fromGregorian 2020 1 1
dato2 = fromGregorian 2020 1 15

-- Sammenligner datoene og lagrer differansen i en variabel
dager = diffDays dato2 dato1

-- Printer ut resultatet
putStrLn $ "Antall dager mellom " ++ show dato1 ++ " og " ++ show dato2 ++ " er " ++ show dager ++ " dager."
```

Dette vil resultere i følgende utskrift:

```
Antall dager mellom 2020-01-01 og 2020-01-15 er 14 dager.
```

I dette eksempelet brukte vi `fromGregorian` funksjonen fra samme bibliotek for å opprette to `Day` objekter. Det er flere måter å opprette `Day` objekter på, avhengig av hva slags format du foretrekker å bruke for datoene. Du kan også bruke `getCurrentTime` funksjonen for å hente nåværende dato og deretter bruke `diffDays` for å sammenligne den med en annen dato.

## Dypdykk

Som nevnt tidligere, kan `diffDays` funksjonen bare sammenligne datoer som `Day` objekter. Men hva om du trenger å sammenligne to datoer i et annet format, for eksempel `UTCTime`? I så fall kan du bruke funksjonen `utctDay` fra `Data.Time.Clock` biblioteket for å konvertere `UTCTime` objekter til `Day` objekter og deretter bruke `diffDays` for å sammenligne dem.

Du kan også bruke `Ord` typeklassen til å sammenligne to `Day` objekter. Dette gjør det mulig å bruke funksjoner som `>`, `<`, `==`, etc. for å sammenligne datoene. Men vær oppmerksom på at disse funksjonene sammenligner nøyaktig dato og ikke tar hensyn til tidspunkt.

## Se Også

- [Haskell Programming Language](https://www.haskell.org/)
- [Data.Time.Calendar Documentation](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html)
- [Data.Time.Clock Documentation](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html)