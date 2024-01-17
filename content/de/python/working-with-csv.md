---
title:                "Arbeiten mit CSV"
html_title:           "Python: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-csv.md"
---

{{< edit_this_page >}}

# Was & warum?
Bei CSV handelt es sich um ein Dateiformat, das verwendet wird, um Daten in Tabellenform zu speichern. CSV steht für "Comma Separated Values" und ist in der Programmierung weit verbreitet. Es ermöglicht uns, Daten in einfachen Textdateien zu speichern, die sowohl von Menschen als auch von Maschinen leicht lesbar sind. Programmierer nutzen CSV, um Daten in einer organisierten und strukturierten Weise zu speichern und zu verarbeiten.

# Wie geht das?
In Python gibt es verschiedene Möglichkeiten, mit CSV-Dateien zu arbeiten. Zuerst müssen wir die eingebaute `csv` Bibliothek importieren. Dann können wir eine CSV-Datei öffnen und die Daten in eine Liste von Listen oder ein Pandas DataFrame lesen. Zum Beispiel:
```python
import csv
with open('daten.csv', 'r') as csv_datei:
    csv_reader = csv.reader(csv_datei)
    for zeile in csv_reader:
        print(zeile)
```
Die Ausgabe wird jede Zeile der CSV-Datei als Liste von Werten anzeigen.

# Tiefer eintauchen
CSV wurde in den 1970er Jahren entwickelt, um den Austausch von Daten zwischen verschiedenen Computersystemen zu erleichtern. Es ist ein einfaches und weit verbreitetes Dateiformat, das in vielen Anwendungen wie Tabellenkalkulationen, Datenbanken und Programmiersprachen verwendet wird. Alternativen zu CSV sind zum Beispiel JSON und XML, die jedoch komplexere Strukturen erlauben.

Beim Lesen oder Schreiben von CSV-Dateien müssen wir uns bewusst sein, dass die Daten nicht typisiert sind, das heißt, sie haben keinen festgelegten Datentyp wie z.B. String oder Integer. Wir müssen daher selbst sicherstellen, dass die Daten richtig formatiert sind, um Fehler bei der Verarbeitung zu vermeiden.

# Siehe auch
Weitere Informationen und Beispiele für die Arbeit mit CSV in Python finden Sie in der offiziellen [Dokumentation der `csv` Bibliothek](https://docs.python.org/3/library/csv.html). Für die Verarbeitung von größeren oder komplexeren Datenmengen ist es empfehlenswert, die leistungsstärkere `pandas` Bibliothek zu verwenden. Eine Einführung in die Arbeit mit Pandas und CSV gibt es [hier](https://realpython.com/python-csv/#parsing-csv-files-with-pandas).