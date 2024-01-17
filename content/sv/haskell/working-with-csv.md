---
title:                "Arbeta med csv"
html_title:           "Haskell: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV är en vanlig filformat som används för att lagra och hantera tabulära data. Det är ett praktiskt sätt för program att kommunicera och utbyta data, särskilt när dessa program är skrivna i olika språk eller använder olika plattformar. Program som hanterar stora mängder data, som till exempel databaser eller statistikverktyg, använder ofta CSV för att enkelt kunna importera och exportera data.

## Så här gör du:
Att arbeta med CSV-filer i Haskell är relativt enkelt, tack vare biblioteket "Data.Csv". Först behöver du importera biblioteket genom att skriva ```Haskell
import Data.Csv
```

För att läsa in en CSV-fil, kan du använda funktionen ```Haskell
parseCsvFromFile :: FilePath -> CSVSettings -> IO (Either String (Vector (Vector ByteString)))
```
Denna funktion tar två argument: sökvägen till din CSV-fil och en inställningsparameter för hur filen ska tolkas. Om tolkningen lyckas, returneras en vektor av vektorer som innehåller data från filen.

För att skriva data till en CSV-fil, använder du funktionen ```Haskell
writeFileUtf8 :: FilePath -> [ByteString] -> IO ()
```
Första argumentet är sökvägen till din fil och andra argumentet är en lista av ByteString som innehåller data att skriva till filen.

Ett exempel på hur du kan läsa in och skriva till en CSV-fil i Haskell:

```Haskell
import Data.Csv

main :: IO ()
main = do
  csvData <- parseCsvFromFile "exempelfil.csv" defaultEncodeOptions -- Läs in filen
  case csvData of
    Left err -> putStrLn $ "Det gick inte att läsa in filen: " ++ err
    Right vec -> do
      let newData = map (map toUpper) vec -- Konvertera datan till versaler
      writeFileUtf8 "nyaexempelfil.csv" $ encode newData -- Skriv data till ny fil
```

Output i "nyaexempelfil.csv" kommer se ut så här:

```csv
RAD1,RAD2,RAD3
DATA1,DATA2,DATA3
```

## Djupdykning:
CSV står för "Comma-Separated Values" och används oftast för att representera en tabell där data är separerad med kommatecken. Det finns dock inga strikta regler för hur CSV-filer ska formateras, vilket kan leda till problem med att tolka filer som skrivits av olika program.

Det finns även alternativ till CSV, som till exempel JSON och XML, som också används för att hantera tabulära data. Användningen av dessa beror ofta på vilket språk och plattform som används och vilka behov som finns för datautbyte.

Även om CSV-filer är relativt enkla att läsa och skriva, finns det en del detaljer som är värda att känna till vid implementering. Till exempel bör du vara uppmärksam på att värden med specialtecken behöver escapade så att de inte tolkas som separerare och att filer kan ha olika teckenkodningar.

## Se även:
- [Data.Csv dokumentation](https://hackage.haskell.org/package/cassava/docs/Data-Csv.html)
- [Comma-Separated Values på Wikipedia](https://en.wikipedia.org/wiki/Comma-separated_values)