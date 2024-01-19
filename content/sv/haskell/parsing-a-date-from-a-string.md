---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka ett datum från en sträng är en operation där en sträng, som representerar ett visst datum, omvandlas till en datastruktur som representerar samma datum. Programmerare gör detta för att enkelt och exakt kunna manipulera informationen i datat.

## Hur man gör:

För att göra det i Haskell kan vi använda `parseTimeM` funktionen från `Data.Time` paketet. Här är ett exempel:

```Haskell
import Data.Time

parseDate :: String -> IO UTCTime
parseDate input = parseTimeM True defaultTimeLocale "%Y-%m-%d" input

main :: IO ()
main = do
  putStrLn "Skriv in ett datum (ÅÅÅÅ-MM-DD):"
  input <- getLine
  parsedDate <- parseDate input
  print parsedDate
```

För att få ut datumet skriver vi det i konsolen och får en form av `UTCTime`. 

## Djupdykning:

Historiskt sett har datumtolkning varit viktigt för att omvandla mänskligt läsbara datum till något datorer kan bearbeta.
Det finns också alternativ till `Data.Time` paketet som `time` paketet eller ens att skapa din egen parser med `Parsec` eller `Megaparsec` bibliotek. 
`parseTimeM` är ett högnivå API som utnyttjar `ParseTime` typklassen. Den exakta implementationen av `parseTimeM` ändras beroende på det underliggande typen som vi försöker tolka.

## Se även:

För mer information kan du kolla på dessa sidor:
- Data.Time paketet dokumentation: http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html
- Introduktion till Parsec: https://wiki.haskell.org/Parsing_a_simple_imperative_language
- Megaparsec dokumentation: https://hackage.haskell.org/package/megaparsec-9.0.1
- Tid och datum hantering i Haskell: http://chrisdone.com/posts/haskell-time