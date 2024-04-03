---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:50.895109-07:00
description: "C\xF3mo: Python proporciona el m\xF3dulo integrado `csv` para manejar\
  \ archivos CSV, lo que facilita leerlos y escribir en ellos. Para una manipulaci\xF3\
  n de datos\u2026"
lastmod: '2024-03-13T22:44:58.636774-06:00'
model: gpt-4-0125-preview
summary: "Python proporciona el m\xF3dulo integrado `csv` para manejar archivos CSV,\
  \ lo que facilita leerlos y escribir en ellos."
title: Trabajando con CSV
weight: 37
---

## Cómo:
Python proporciona el módulo integrado `csv` para manejar archivos CSV, lo que facilita leerlos y escribir en ellos. Para una manipulación de datos más robusta y compleja, la biblioteca de terceros `pandas` es muy popular.

### Usando el módulo `csv`


#### Leyendo un archivo CSV
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*Suponiendo que `sample.csv` contiene:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*Salida:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### Escribiendo en un archivo CSV
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*Crea o sobrescribe `output.csv` con:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### Usando `pandas` para CSV
`pandas` es una poderosa biblioteca para la manipulación de datos que simplifica trabajar con archivos CSV entre otros formatos de datos.

#### Instalar pandas
```shell
pip install pandas
```

#### Leyendo un archivo CSV con pandas
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*Salida:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### Escribiendo en un archivo CSV con pandas
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*Crea o sobrescribe `output_pandas.csv` con:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
