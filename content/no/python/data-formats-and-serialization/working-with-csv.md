---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:07.117359-07:00
description: "\xC5 jobbe med CSV (kommaseparerte verdier) inneb\xE6rer lesing fra\
  \ og skriving til CSV-filer, et vanlig format for lagring av tabul\xE6re data. Programmerere\
  \ gj\xF8r\u2026"
lastmod: '2024-02-25T18:49:38.610910-07:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med CSV (kommaseparerte verdier) inneb\xE6rer lesing fra og skriving\
  \ til CSV-filer, et vanlig format for lagring av tabul\xE6re data. Programmerere\
  \ gj\xF8r\u2026"
title: Arbeide med CSV
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med CSV (kommaseparerte verdier) innebærer lesing fra og skriving til CSV-filer, et vanlig format for lagring av tabulære data. Programmerere gjør dette for å enkelt utveksle og lagre data i et enkelt, tekstbasert format som er bredt støttet på tvers av forskjellige plattformer og språk.

## Hvordan:
Python tilbyr den innebygde `csv`-modulen for å håndtere CSV-filer, noe som gjør det enkelt å lese fra og skrive til dem. For mer robuste og komplekse datahåndteringer er tredjepartsbiblioteket `pandas` svært populært.

### Bruke `csv`-modulen

#### Lese en CSV-fil
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*Antar at `sample.csv` inneholder:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*Utdata:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### Skrive til en CSV-fil
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*Oppretter eller overskriver `output.csv` med:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### Bruke `pandas` for CSV
`pandas` er et kraftig bibliotek for datahåndtering som forenkler arbeid med CSV-filer blant andre dataformater.

#### Installere pandas
```shell
pip install pandas
```

#### Lese en CSV-fil med pandas
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*Utdata:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### Skrive til en CSV-fil med pandas
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*Oppretter eller overskriver `output_pandas.csv` med:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
