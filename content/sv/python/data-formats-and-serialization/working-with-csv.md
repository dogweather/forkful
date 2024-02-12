---
title:                "Arbeta med CSV"
aliases:
- /sv/python/working-with-csv/
date:                  2024-02-03T19:21:02.437181-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeta med CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med CSV (Comma-Separated Values) innebär att läsa från och skriva data till CSV-filer, ett vanligt format för att lagra tabulär data. Programmerare gör det för att enkelt utbyta och lagra data i ett enkelt, textbaserat format som är brett stöttat över olika plattformar och språk.

## Hur:
Python tillhandahåller den inbyggda `csv` modulen för att hantera CSV-filer, vilket gör det enkelt att läsa från och skriva till dem. För mer robust och komplex datamanipulation är det tredjepartsbiblioteket `pandas` mycket populärt.

### Använda `csv` modulen

#### Läsa en CSV-fil
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*Antar att `sample.csv` innehåller:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*Utskrift:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### Skriva till en CSV-fil
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*Skapar eller skriver över `output.csv` med:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### Använda `pandas` för CSV
`pandas` är ett kraftfullt bibliotek för datamanipulation som förenklar arbete med CSV-filer bland andra dataformat.

#### Installera pandas
```shell
pip install pandas
```

#### Läsa en CSV-fil med pandas
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*Utskrift:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### Skriva till en CSV-fil med pandas
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*Skapar eller skriver över `output_pandas.csv` med:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
