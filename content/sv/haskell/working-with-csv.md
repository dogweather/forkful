---
title:                "Att arbeta med csv"
html_title:           "Haskell: Att arbeta med csv"
simple_title:         "Att arbeta med csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV (Comma-Separated Values) är ett vanligt format för att lagra tabellliknande data och är ofta användbart för att dela och analysera information. Att kunna hantera CSV-data i Haskell ger dig en kraftfull verktygslåda för att manipulera och bearbeta data direkt i ditt program.

## Hur man gör det

För att börja arbeta med CSV-data i Haskell behöver du importera "csv"-modulen. Sedan kan du använda funktionerna ```import :: FilePath -> IO (Either String CSV)``` för att importera en CSV-fil och ```export :: Csv -> FilePath -> IO ()``` för att exportera en CSV-fil.

```Haskell
import Text.CSV

-- Importera en CSV-fil
main = do
  result <- import "data.csv"
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right csv -> print csv

-- Exportera en CSV-fil
main = do
  let csv = [["John", "Doe", "30"], ["Jane", "Smith", "25"]]
  export csv "data.csv"
```

Output för ```import "data.csv"``` skulle se ut som följande:

```Haskell
Right [["John","Doe","30"],["Jane","Smith","25"]]
```

## Djupdykning

För mer avancerade CSV-manipulationer finns det många funktioner tillgängliga i "csv"-modulen. Här är några exempel med en kort beskrivning:

- `parseCSV :: CSVSettings -> String -> Either String CSV` för att konvertera en sträng till en CSV-datastruktur.
- `writeCSV :: CSVSettings -> FilePath -> CSV -> IO ()` för att skriva en CSV-datastruktur till en fil.
- `encode :: CSV -> ByteString` för att generera en enkel CSV-sträng från en CSV-datastruktur.
- `decodeWith :: CSVSettings -> ByteString -> Either String CSV` för att konvertera en bytesträng till en CSV-datastruktur med hjälp av anpassade inställningar.

Det finns också funktioner för att manipulera data, såsom `insertRow` för att lägga till en rad, `append` för att lägga till en hel CSV till en annan och `filter` för att välja ut vissa rader baserat på ett villkor.

## Se också

- [Officiell Hackage-sida för "csv"-modulen](https://hackage.haskell.org/package/csv)
- [En enkel guide till CSV-hantering i Haskell](https://somashekhar.com/haskell/csv-handling/)
- [En interaktiv online-lektion om CSV i Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple-csv-file-parser)