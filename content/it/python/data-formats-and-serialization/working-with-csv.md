---
title:                "Lavorare con i CSV"
aliases: - /it/python/working-with-csv.md
date:                  2024-02-03T19:20:52.130912-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con i CSV (Valori Separati da Virgola) comporta la lettura da e la scrittura su file CSV, un formato comune per memorizzare dati tabellari. I programmatori lo fanno per scambiare e memorizzare facilmente dati in un formato basato su testo semplice, ampiamente supportato su diverse piattaforme e linguaggi.

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
