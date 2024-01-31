---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
simple_title:         "Arbeiten mit CSV-Dateien"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV steht für "Comma-Separated Values" und ist ein Dateiformat, das für den Datenaustausch genutzt wird. Programmierer verwenden es, weil es einfach, menschenlesbar und weit verbreitet ist.

## How to:
Um mit CSV in Python zu arbeiten, verwenden wir das `csv` Modul. So liest und schreibst du CSV-Dateien:

```Python
import csv

# CSV-Datei lesen
with open('beispiel.csv', mode='r', encoding='utf-8') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)

# CSV-Datei schreiben
with open('beispiel_neu.csv', mode='w', encoding='utf-8', newline='') as file:
    csv_writer = csv.writer(file)
    csv_writer.writerow(['Spalte1', 'Spalte2', 'Spalte3'])
    csv_writer.writerow(['Daten1', 'Daten2', 'Daten3'])
```

Ausgabe beim Lesen könnte sein:
```
['Spalte1', 'Spalte2', 'Spalte3']
['Daten1', 'Daten2', 'Daten3']
```

## Deep Dive
CSV-Dateien wurden Anfang der 1970er Jahre populär. Alternativen sind etwa JSON oder XML, die beide strukturiertere Daten und komplexe Hierarchien ermöglichen. CSV überzeugt durch Einfachheit und Geschwindigkeit, kann aber bei komplexen Datenstrukturen schnell unübersichtlich werden.

## See Also
- Python `csv` Modul-Dokumentation: https://docs.python.org/3/library/csv.html
- W3Schools-Anleitung zu CSV in Python: https://www.w3schools.com/python/python_csv.asp
- CSV vs. JSON: https://www.datacamp.com/community/tutorials/json-vs-csv
