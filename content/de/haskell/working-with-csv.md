---
title:                "Haskell: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV-Dateien sind eine häufig genutzte und universelle Datenformat für den Austausch von Tabellen und Daten in der Programmierung. In Haskell gibt es viele nützliche Funktionen und Libraries für das Lesen und Schreiben von CSV-Dateien. In diesem Blog-Post werden wir uns genauer damit beschäftigen, wie man mit CSV-Dateien in Haskell arbeiten kann.

## Wie man mit CSV-Dateien in Haskell arbeitet

Das Lesen und Schreiben von CSV-Dateien in Haskell ist relativ einfach und erfordert nur wenige Code-Zeilen. Zunächst müssen wir jedoch die "csv"-Bibliothek importieren, die uns die nötigen Funktionen dafür zur Verfügung stellt.

```Haskell
import Text.CSV
```

Um eine CSV-Datei zu lesen, können wir die Funktion `parseCSVFromFile` verwenden, die uns eine Liste von Listen zurückgibt, die die einzelnen Zeilen und Einträge in der CSV-Datei darstellen.

```Haskell
main = do
    csvData <- parseCSVFromFile "example.csv"
    case csvData of
        Left err -> putStrLn "Fehler beim Lesen der CSV-Datei:"
        Right csv -> print csv
```

Dieser Code liest die CSV-Datei "example.csv" ein und gibt sie als Liste von Listen von Strings aus. Um die Daten weiter zu verarbeiten, können wir beispielsweise die `map`-Funktion verwenden, um über die Zeilen zu iterieren und die entsprechenden Einträge auszuwählen.

```Haskell
main = do
    csvData <- parseCSVFromFile "example.csv"
    case csvData of
        Left err -> putStrLn "Fehler beim Lesen der CSV-Datei:"
        Right csv -> mapM_ putStrLn (map (!!1) csv)
```

Dieser Code gibt die Einträge aus der zweiten Spalte der CSV-Datei auf der Konsole aus. Wir können auch CSV-Dateien erstellen, indem wir eine Liste von Listen von Strings erstellen und sie mit der `writeFile`-Funktion in eine Datei schreiben. Hier ist ein einfaches Beispiel:

```Haskell
main = do
    let csvData = [["Name", "Alter"], ["Max", "25"], ["Maria", "30"], ["Tom", "28"]]
    writeFile "new.csv" (printCSV csvData)
```

Dieser Code erstellt eine CSV-Datei mit dem Namen "new.csv" und den angegebenen Daten.

## Tiefgehende Analyse

Die "csv"-Bibliothek bietet noch viele weitere nützliche Funktionen für das Arbeiten mit CSV-Dateien. Beispielsweise können wir mit der `csvHeaders`-Funktion die Spaltennamen einer CSV-Datei extrahieren oder mit der `filterCSV`-Funktion bestimmte Zeilen basierend auf Bedingungen auswählen.

Es ist auch möglich, mit der `readCSV`-Funktion direkt aus einer Zeichenkette zu lesen oder mit der `writeCSV`-Funktion direkt in eine Zeichenkette zu schreiben.

Für weitere Informationen zu allen verfügbaren Funktionen und deren Anwendung empfehle ich, die offizielle Dokumentation der "csv"-Bibliothek zu lesen.

## Siehe auch

- [Die offizielle Dokumentation der "csv"-Bibliothek](https://hackage.haskell.org/package/csv)
- [Ein Einsteiger-Tutorial zu Haskell](https://www.haskell.org/tutorial/)
- [Einführung in CSV-Dateien in Python](https://realpython.com/python-csv/)