---
title:                "Arbeiten mit csv"
html_title:           "Haskell: Arbeiten mit csv"
simple_title:         "Arbeiten mit csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?

CSV (Comma Separated Values) ist ein gebräuchliches Dateiformat zum Speichern von tabellarischen Daten. Es wird häufig von Programmierern verwendet, um Daten von einer Anwendung zur anderen zu übertragen oder um Daten in einem strukturierten Format zu speichern. CSV ist einfach zu lesen und zu schreiben, was es zu einer beliebten Wahl für die Verwendung in der Programmierung macht.

## Wie geht's?

Das Lesen und Schreiben von CSV-Dateien in Haskell ist sehr einfach. Hier sind ein paar Beispiele, wie man das mit der beliebten Bibliothek `cassava` machen kann:

```Haskell
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Control.Applicative ((<*>), (<$>))

main = do
  csvFile <- BL.readFile "beispiel.csv"
  case decode NoHeader csvFile of
    Left err -> putStrLn err
    Right rows -> BL.putStr $ encode rows
```

Dieses Beispiel liest eine CSV-Datei namens "beispiel.csv" und gibt die Inhalte auf der Konsole aus. Es verwendet die Funktion `decode` aus der `cassava`-Bibliothek, um die CSV-Datei in eine Liste von Zeilen umzuwandeln. Diese kann dann mit der `encode`-Funktion wieder in eine CSV-Datei zurückgeschrieben werden.

```Haskell
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Vector (toList)

main = do
  csvFile <- BL.readFile "beispiel.csv"
  case decode NoHeader csvFile of
    Left err -> putStrLn err
    Right rows -> mapM_ (putStrLn . unwords . toList) rows
```

Dieses Beispiel liest die CSV-Datei wieder ein, wandelt sie aber diesmal in eine Liste von Listen um. Jede Zeile wird dann mit der `unwords`-Funktion in einen String umgewandelt und auf der Konsole ausgegeben.

## Tiefer tauchen

CSV wurde in den 1970er Jahren entwickelt und war ursprünglich ein einfaches Dateiformat, um Daten zwischen verschiedenen Tabellenkalkulationsprogrammen auszutauschen. Heutzutage wird es jedoch nicht nur für den Austausch von Daten verwendet, sondern auch für die Speicherung großer Datenmengen, da es einfach zu lesen und zu schreiben ist.

Als Alternative zu CSV gibt es das ähnliche Format TSV (Tab Separated Values), bei dem statt Kommas Tabs als Trennzeichen verwendet werden. Dies kann nützlich sein, wenn die zu speichernden Daten bereits Kommas enthalten.

In Haskell gibt es verschiedene Bibliotheken für das Lesen und Schreiben von CSV-Dateien, wie z.B. `cassava`, `records-csv` oder `pipes-csv`. Jede Bibliothek hat ihre eigenen Stärken und Schwächen, aber im Allgemeinen sind sie alle benutzerfreundlich und einfach zu verwenden.

## Siehe auch

- Offizielle Dokumentation von `cassava`: https://hackage.haskell.org/package/cassava/docs/Data-Csv.html
- `records-csv` Bibliothek: https://github.com/alephnullplex/records-csv
- `pipes-csv` Bibliothek: https://github.com/michaelt/pipes-csv