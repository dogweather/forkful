---
title:                "Робота з CSV"
aliases:
- uk/python/working-with-csv.md
date:                  2024-02-03T19:21:18.982231-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Робота із CSV (Comma-Separated Values, значення, розділені комами) передбачає читання з файлів CSV та запис у них даних, це поширений формат для зберігання табличних даних. Програмісти роблять це для легкого обміну та зберігання даних у простому текстовому форматі, який широко підтримується на різних платформах і мовами.

## Як це зробити:
Python надає вбудований модуль `csv` для роботи з файлами CSV, що робить читання з них та запис у них простим. Для більш міцної та складної маніпуляції з даними високої популярності набула стороння бібліотека `pandas`.

### Використання модуля `csv`

#### Читання файлу CSV
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*Припустимо, що `sample.csv` містить:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*Вивід:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### Запис у файл CSV
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*Створює або перезаписує `output.csv` з:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### Використання `pandas` для CSV
`pandas` - потужна бібліотека для маніпулювання даними, яка спрощує роботу з файлами CSV серед інших форматів даних.

#### Встановлення pandas
```shell
pip install pandas
```

#### Читання файлу CSV з pandas
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*Вивід:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### Запис у файл CSV з pandas
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*Створює або перезаписує `output_pandas.csv` з:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
