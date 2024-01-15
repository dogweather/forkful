---
title:                "Arbeiten mit csv"
html_title:           "Python: Arbeiten mit csv"
simple_title:         "Arbeiten mit csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV (Comma Separated Values) ist ein gängiges Dateiformat für den Austausch von Daten zwischen verschiedenen Programmen. Es ist einfach zu erstellen und zu lesen, daher ist es eine beliebte Wahl für den Umgang mit tabellarischen Daten in Python.

## Wie man CSV in Python verwendet

Um mit CSV-Dateien in Python zu arbeiten, müssen wir die `csv`-Bibliothek importieren.
```
import csv
```
Als nächstes müssen wir die CSV-Datei öffnen und den Inhalt in ein `csv.reader`-Objekt konvertieren.
```
with open('beispiel.csv') as csvdatei:
    csv_reader = csv.reader(csvdatei)
```
Jetzt können wir auf die einzelnen Zeilen der CSV-Datei zugreifen, indem wir über das `csv_reader`-Objekt iterieren.
```
for zeile in csv_reader:
    print(zeile)
```
Dies wird jede Zeile der CSV-Datei als Liste von Werten ausgeben.

Wir können auch spezifische Spalten auswählen und in einer neuen Liste speichern.
```
with open('beispiel.csv') as csvdatei:
    csv_reader = csv.reader(csvdatei)
    for zeile in csv_reader:
        name = zeile[0]
        alter = zeile[1]
        print(name + " ist " + alter + " Jahre alt.")
```

Um eine CSV-Datei zu schreiben, müssen wir das `csv.writer`-Objekt verwenden.
```
with open('neue_datei.csv', 'w', newline='') as csvdatei:
    csv_writer = csv.writer(csvdatei)
    csv_writer.writerow(['Name', 'Alter'])    # Überschriften hinzufügen
    csv_writer.writerow(['Tom', '25'])        # Daten hinzufügen
    csv_writer.writerow(['Anna', '30'])
```

## Tiefergehende Informationen

Die `csv`-Bibliothek bietet viele nützliche Funktionen, um mit CSV-Dateien zu arbeiten. Hier sind einige wichtige Dinge, die Sie wissen sollten:

- Wenn Ihre CSV-Datei Trennzeichen verwendet, die nicht eindeutig sind (z.B. ein Tabulator anstelle eines Kommas), können Sie den `csv.reader`- und `csv.writer`-Funktionen das Trennzeichen als Argument übergeben.
- Wenn Sie mit großen CSV-Dateien arbeiten, kann es effizienter sein, das `csv.DictReader`-Objekt zu verwenden, das die Daten in ein Wörterbuch (Dictionary) konvertiert.
- Die `csv`-Bibliothek bietet auch Funktionen zum Lesen und Schreiben von Daten in DictWriter- und DictReader-Objekten, die es einfacher machen, mit komplexen Datenstrukturen umzugehen.

## Siehe Auch

- [Dokumentation der CSV-Bibliothek](https://docs.python.org/3/library/csv.html)
- [Tutorial für den Umgang mit CSV-Dateien in Python](https://realpython.com/python-csv/)
- [CSV Dateien mit Python lesen und schreiben - Ein Tutorial von Pythonbuch](https://pythonbuch.com/kapitel-5-csv-dateien.html)