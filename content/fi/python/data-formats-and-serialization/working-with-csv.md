---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:00.119000-07:00
description: "Ty\xF6skentely CSV:n (pilkuin erotetut arvot) parissa sis\xE4lt\xE4\xE4\
  \ lukemista ja kirjoittamista CSV-tiedostoihin, yleinen muoto tabulaarisen datan\
  \ tallentamiseen.\u2026"
lastmod: '2024-03-13T22:44:56.165889-06:00'
model: gpt-4-0125-preview
summary: "Ty\xF6skentely CSV:n (pilkuin erotetut arvot) parissa sis\xE4lt\xE4\xE4\
  \ lukemista ja kirjoittamista CSV-tiedostoihin, yleinen muoto tabulaarisen datan\
  \ tallentamiseen."
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

## Kuinka:
Python tarjoaa sisäänrakennetun `csv` moduulin CSV-tiedostojen käsittelyyn, tehden lukemisesta ja kirjoittamisesta niihin suoraviivaista. Vankempaan ja monimutkaisempaan datan käsittelyyn suosittu kolmannen osapuolen kirjasto on `pandas`.

### Käyttäen `csv` moduulia


#### CSV-tiedoston lukeminen
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*Olettaen, että `sample.csv` sisältää:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*Tuloste:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### Kirjoittaminen CSV-tiedostoon
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*Luo tai ylikirjoittaa `output.csv`:n kanssa:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### Käyttäen `pandas`ia CSV:lle
`pandas` on tehokas kirjasto datan käsittelyyn, joka yksinkertaistaa työskentelyä CSV-tiedostojen sekä muiden datamuotojen kanssa.

#### Asenna pandas
```shell
pip install pandas
```

#### CSV-tiedoston lukeminen pandasilla
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*Tuloste:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### Kirjoittaminen CSV-tiedostoon pandasilla
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*Luo tai ylikirjoittaa `output_pandas.csv`:n kanssa:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
