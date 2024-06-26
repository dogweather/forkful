---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:52.130912-07:00
description: "Come fare: Python fornisce il modulo integrato `csv` per gestire i file\
  \ CSV, rendendo semplice leggere da e scrivere su di essi. Per una manipolazione\
  \ dei\u2026"
lastmod: '2024-03-13T22:44:43.023388-06:00'
model: gpt-4-0125-preview
summary: Python fornisce il modulo integrato `csv` per gestire i file CSV, rendendo
  semplice leggere da e scrivere su di essi.
title: Lavorare con i CSV
weight: 37
---

## Come fare:
Python fornisce il modulo integrato `csv` per gestire i file CSV, rendendo semplice leggere da e scrivere su di essi. Per una manipolazione dei dati più robusta e complessa, la libreria di terze parti `pandas` è molto popolare.

### Utilizzando il modulo `csv`


#### Leggere un file CSV
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*Assumendo che `sample.csv` contenga:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*Output:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### Scrivere su un file CSV
```python
import csv

righe = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    scrittore = csv.writer(file)
    scrittore.writerows(righe)
```
*Crea o sovrascrive `output.csv` con:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### Utilizzando `pandas` per CSV
`pandas` è una libreria potente per la manipolazione dei dati che semplifica il lavoro con i file CSV tra gli altri formati di dati.

#### Installare pandas
```shell
pip install pandas
```

#### Leggere un file CSV con pandas
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*Output:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### Scrivere su un file CSV con pandas
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*Crea o sovrascrive `output_pandas.csv` con:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
