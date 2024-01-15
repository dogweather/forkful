---
title:                "Arbeiten mit CSV"
html_title:           "Haskell: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

Wenn du jemals mit Daten gearbeitet hast, hast du wahrscheinlich schon einmal von CSV-Dateien gehört. CSV steht für "Comma Separated Values" und ist ein beliebtes Format für den Austausch und die Speicherung von Tabellendaten. In der Welt der Programmierung ist es daher wichtig, auch mit CSV-Dateien umgehen zu können. Hier kommt Haskell ins Spiel - eine funktionale Programmiersprache, die sich perfekt für die Arbeit mit CSV eignet. In diesem Artikel werde ich dir zeigen, warum es sich lohnt, sich mit CSV in Haskell auseinanderzusetzen und wie du das am besten tun kannst.

## Wie funktioniert es?

Um mit CSV-Dateien in Haskell zu arbeiten, benötigen wir das Paket "csv" aus dem Haskell-Bibliothekssystem "Hackage". Wir können es ganz einfach mit dem Befehl `cabal install csv` installieren. Anschließend können wir das Modul "Data.CSV" in unserem Code importieren und mit der Funktion `parseCSVFromFile` unsere CSV-Datei einlesen. Hier ist ein Beispielcode:

```Haskell
import Text.CSV

main = do
    let filename = "data.csv"
    csvData <- parseCSVFromFile filename
    case csvData of
        Left _ -> putStrLn "Error: Die Datei konnte nicht gelesen werden."
        Right csv -> putStrLn "Die Datei wurde erfolgreich gelesen."
```

In diesem Beispiel haben wir eine CSV-Datei mit dem Namen "data.csv" eingelesen. Die Funktion `parseCSVFromFile` gibt entweder einen Fehler oder eine Struktur zurück, die unser CSV-Daten darstellt. Mit etwas Mustermatching können wir nun beispielsweise die erste Zeile unserer CSV-Datei ausgeben:

```Haskell
case csvData of
    Left _ -> putStrLn "Error: Die Datei konnte nicht gelesen werden."
    Right csv -> putStrLn $ show $ head $ tail csv
```

Mit der Funktion `show` können wir die Zeile in einer lesbaren Form ausgeben. Das `tail` entfernt die erste Zeile (da diese in der Regel die Überschriften enthält) und `head` gibt die erste Zeile des Rests zurück - also die tatsächlichen Daten.

## Tiefergehende Informationen

Das Paket "csv" bietet uns noch viele weitere Funktionen, um mit unseren CSV-Daten zu arbeiten. Zum Beispiel können wir die Daten sortieren, filtern oder in ein anderes Format umwandeln. Auch das Schreiben von CSV-Dateien ist möglich. Für eine detailliertere Beschreibung der verfügbaren Funktionen empfehle ich dir, die Dokumentation auf Hackage zu lesen (siehe "Siehe auch"). Eines solltest du jedoch beachten: Wenn deine CSV-Datei Kommazahlen enthält, wird Haskell sie standardmäßig als Bruchzahlen behandeln. Um dies zu vermeiden, solltest du die Funktion `dequote` verwenden, um die Zahlen in Strings umzuwandeln.

## Siehe auch

Dokumentation des "csv" Pakets auf Hackage: https://hackage.haskell.org/package/csv

Eine Einführung in Haskell (in englischer Sprache): https://wiki.haskell.org/Introduction

Eine interaktive Online-Umgebung, um Haskell-Code auszuprobieren: http://tryhaskell.org/