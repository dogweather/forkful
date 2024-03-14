---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:04.591923-07:00
description: "Att arbeta med CSV-filer (Comma-Separated Values eller komma-separerade\
  \ v\xE4rden) inneb\xE4r att tolka och generera filer som lagrar tabul\xE4r data\
  \ i ett enkelt,\u2026"
lastmod: '2024-03-13T22:44:37.977135-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med CSV-filer (Comma-Separated Values eller komma-separerade\
  \ v\xE4rden) inneb\xE4r att tolka och generera filer som lagrar tabul\xE4r data\
  \ i ett enkelt,\u2026"
title: Arbeta med CSV
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med CSV-filer (Comma-Separated Values eller komma-separerade värden) innebär att tolka och generera filer som lagrar tabulär data i ett enkelt, textbaserat format. Programmerare sysslar ofta med denna uppgift för att effektivt importera eller exportera data från kalkylark, databaser, eller för att underlätta datautbyte mellan olika program.

## Hur:

I Haskell kan hantering av CSV-filer uppnås med hjälp av `cassava`-biblioteket, ett av de populära tredjepartsbiblioteken för detta ändamål. Nedan finns exempel som visar hur man läser från och skriver till CSV-filer med `cassava`.

**1. Läsa en CSV-fil:**

Se först till att du har `cassava` installerat genom att lägga till det i ditt projekts cabal-fil eller använda Stack.

Här är ett enkelt exempel på att läsa en CSV-fil och skriva ut varje post. Vi antar att CSV-filen har två kolumner: namn och ålder.

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
        Right v -> V.forM_ v $ \(namn, ålder) ->
            putStrLn $ namn ++ " är " ++ show (ålder :: Int) ++ " år gammal."
```

Antag att `people.csv` innehåller:
```
John,30
Jane,25
```
Utmatningen blir:
```
John är 30 år gammal.
Jane är 25 år gammal.
```

**2. Skriva en CSV-fil:**

För att skapa en CSV-fil kan du använda `encode`-funktionen från `cassava`.

Så här kan du skriva en lista med poster till en CSV-fil:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

Efter att ha kört detta program kommer `output.csv` att innehålla:

```
John,30
Jane,25
```

Denna kortfattade introduktion till att arbeta med CSV-filer i Haskell med hjälp av `cassava`-biblioteket demonstrerar hur man både läser från och skriver till CSV-filer, vilket gör datamanipuleringsuppgifter mer tillgängliga för de som är nya till språket.
