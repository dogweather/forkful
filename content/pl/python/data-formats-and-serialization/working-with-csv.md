---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:03.767104-07:00
description: "Jak to robi\u0107: Python oferuje wbudowany modu\u0142 `csv`, kt\xF3\
  ry pozwala na obs\u0142ug\u0119 plik\xF3w CSV, co sprawia, \u017Ce odczyt z nich\
  \ i zapisywanie do nich jest prostsze.\u2026"
lastmod: '2024-03-13T22:44:34.973637-06:00'
model: gpt-4-0125-preview
summary: "Python oferuje wbudowany modu\u0142 `csv`, kt\xF3ry pozwala na obs\u0142\
  ug\u0119 plik\xF3w CSV, co sprawia, \u017Ce odczyt z nich i zapisywanie do nich\
  \ jest prostsze."
title: Praca z plikami CSV
weight: 37
---

## Jak to robić:
Python oferuje wbudowany moduł `csv`, który pozwala na obsługę plików CSV, co sprawia, że odczyt z nich i zapisywanie do nich jest prostsze. Dla bardziej zaawansowanych i złożonych manipulacji danymi, popularna jest biblioteka zewnętrzna `pandas`.

### Korzystanie z modułu `csv`


#### Odczyt pliku CSV
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*Zakładając, że `sample.csv` zawiera:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*Wynik:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### Zapis do pliku CSV
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*Tworzy lub nadpisuje `output.csv` z:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### Korzystanie z `pandas` dla CSV
`pandas` to potężna biblioteka do manipulowania danymi, która upraszcza pracę z plikami CSV między innymi formatami danych.

#### Instalacja pandas
```shell
pip install pandas
```

#### Odczyt pliku CSV z użyciem pandas
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*Wynik:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### Zapis do pliku CSV z użyciem pandas
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*Tworzy lub nadpisuje `output_pandas.csv` z:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
