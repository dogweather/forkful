---
title:                "Arbeiten mit CSV"
aliases: - /de/python/working-with-csv.md
date:                  2024-02-03T19:20:48.364723-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit CSV (Comma-Separated Values, durch Kommas getrennte Werte) umfasst das Lesen von und das Schreiben in CSV-Dateien, einem gängigen Format zur Speicherung von tabellarischen Daten. Programmierer tun dies, um Daten einfach auszutauschen und in einem einfachen, textbasierten Format zu speichern, das auf verschiedenen Plattformen und in verschiedenen Sprachen weit verbreitet unterstützt wird.

## Wie:
Python bietet das eingebaute `csv`-Modul an, um mit CSV-Dateien zu arbeiten, was das Lesen von und das Schreiben in sie unkompliziert macht. Für robustere und komplexere Datenmanipulationen ist die Drittanbieterbibliothek `pandas` sehr beliebt.

### Verwendung des `csv`-Moduls

#### Ein CSV-Datei lesen
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*Angenommen, `sample.csv` enthält:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*Ausgabe:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### In eine CSV-Datei schreiben
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*Erstellt oder überschreibt `output.csv` mit:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### Verwendung von `pandas` für CSV
`pandas` ist eine leistungsfähige Bibliothek für die Datenmanipulation, die die Arbeit mit CSV-Dateien unter anderen Datenformaten vereinfacht.

#### pandas installieren
```shell
pip install pandas
```

#### Eine CSV-Datei mit pandas lesen
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*Ausgabe:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### In eine CSV-Datei mit pandas schreiben
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*Erstellt oder überschreibt `output_pandas.csv` mit:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
