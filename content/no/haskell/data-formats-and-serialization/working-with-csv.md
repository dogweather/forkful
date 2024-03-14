---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:48.660182-07:00
description: "\xC5 jobbe med CSV-filer (kommaseparerte verdier) inneb\xE6rer parsing\
  \ og generering av filer som lagrer tabelldata i et enkelt, tekstbasert format.\u2026"
lastmod: '2024-03-13T22:44:40.865935-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med CSV-filer (kommaseparerte verdier) inneb\xE6rer parsing og\
  \ generering av filer som lagrer tabelldata i et enkelt, tekstbasert format.\u2026"
title: Arbeide med CSV
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å jobbe med CSV-filer (kommaseparerte verdier) innebærer parsing og generering av filer som lagrer tabelldata i et enkelt, tekstbasert format. Programmerere engasjerer seg ofte i denne oppgaven for effektivt å importere eller eksportere data fra regneark, databaser, eller for å legge til rette for datadeling mellom ulike programmer.

## Hvordan:

I Haskell kan behandling av CSV-filer oppnås ved å bruke `cassava`-biblioteket, et av de populære tredjepartsbibliotekene for dette formålet. Nedenfor er eksempler som viser hvordan man leser fra og skriver til CSV-filer ved hjelp av `cassava`.

**1. Lese en CSV-fil:**

Først, sørg for at du har `cassava` installert ved å legge det til i prosjektets cabal-fil eller ved å bruke Stack.

Her er et enkelt eksempel for å lese en CSV-fil og skrive ut hver post. Vi antar at CSV-filen har to kolonner: navn og alder.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(navn, alder) ->
            putStrLn $ navn ++ " er " ++ show (alder :: Int) ++ " år gammel."
```

Antar at `people.csv` inneholder:
```
John,30
Jane,25
```
Utdata vil være:
```
John er 30 år gammel.
Jane er 25 år gammel.
```

**2. Skrive en CSV-fil:**

For å opprette en CSV-fil, kan du bruke `encode`-funksjonen fra `cassava`.

Slik kan du skrive en liste med poster til en CSV-fil:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

Etter å ha kjørt dette programmet, vil `output.csv` inneholde:

```
John,30
Jane,25
```

Denne korte introduksjonen til arbeid med CSV-filer i Haskell ved hjelp av `cassava`-biblioteket viser hvordan å lese fra og skrive til CSV-filer, noe som gjør datahåndteringsoppgaver mer tilgjengelige for de som er nye til språket.
