---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:48.364723-07:00
description: "Die Arbeit mit CSV (Comma-Separated Values, durch Kommas getrennte Werte)\
  \ umfasst das Lesen von und das Schreiben in CSV-Dateien, einem g\xE4ngigen Format\u2026"
lastmod: '2024-03-13T22:44:53.400939-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit CSV (Comma-Separated Values, durch Kommas getrennte Werte)\
  \ umfasst das Lesen von und das Schreiben in CSV-Dateien, einem g\xE4ngigen Format\
  \ zur Speicherung von tabellarischen Daten."
title: Arbeiten mit CSV
weight: 37
---

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
