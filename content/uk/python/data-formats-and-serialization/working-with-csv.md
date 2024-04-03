---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:18.982231-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Python \u043D\u0430\u0434\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\
  \u043D\u0438\u0439 \u043C\u043E\u0434\u0443\u043B\u044C `csv` \u0434\u043B\u044F\
  \ \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 \u0444\u0430\u0439\u043B\u0430\u043C\
  \u0438 CSV, \u0449\u043E \u0440\u043E\u0431\u0438\u0442\u044C \u0447\u0438\u0442\
  \u0430\u043D\u043D\u044F \u0437 \u043D\u0438\u0445 \u0442\u0430 \u0437\u0430\u043F\
  \u0438\u0441 \u0443 \u043D\u0438\u0445 \u043F\u0440\u043E\u0441\u0442\u0438\u043C\
  . \u0414\u043B\u044F \u0431\u0456\u043B\u044C\u0448 \u043C\u0456\u0446\u043D\u043E\
  \u0457 \u0442\u0430 \u0441\u043A\u043B\u0430\u0434\u043D\u043E\u0457\u2026"
lastmod: '2024-03-13T22:44:48.619667-06:00'
model: gpt-4-0125-preview
summary: "Python \u043D\u0430\u0434\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u0438\u0439 \u043C\u043E\u0434\u0443\u043B\u044C `csv` \u0434\u043B\
  \u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 \u0444\u0430\u0439\u043B\u0430\
  \u043C\u0438 CSV, \u0449\u043E \u0440\u043E\u0431\u0438\u0442\u044C \u0447\u0438\
  \u0442\u0430\u043D\u043D\u044F \u0437 \u043D\u0438\u0445 \u0442\u0430 \u0437\u0430\
  \u043F\u0438\u0441 \u0443 \u043D\u0438\u0445 \u043F\u0440\u043E\u0441\u0442\u0438\
  \u043C."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
weight: 37
---

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
