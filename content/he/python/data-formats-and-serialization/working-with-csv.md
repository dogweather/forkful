---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:13.744336-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E4\u05D9\u05D9\
  \u05EA\u05D5\u05DF \u05DE\u05E1\u05E4\u05E7\u05EA \u05D0\u05EA \u05DE\u05D5\u05D3\
  \u05D5\u05DC \u05D4-`csv` \u05D4\u05DE\u05D5\u05D1\u05E0\u05D4 \u05DC\u05D8\u05D9\
  \u05E4\u05D5\u05DC \u05D1\u05E7\u05D1\u05E6\u05D9 CSV, \u05D5\u05D4\u05D5\u05E4\u05DB\
  \u05EA \u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\u05D4\u05DD \u05D5\u05DB\
  \u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\u05D4\u05DD \u05DC\u05E4\u05E9\u05D5\
  \u05D8\u05D4. \u05DC\u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\u05E6\u05D9\u05D5\u05EA\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D7\u05D6\u05E7\u05D5\u05EA \u05D5\u05DE\
  \u05D5\u05E8\u05DB\u05D1\u05D5\u05EA \u05D9\u05D5\u05EA\u05E8, \u05D4\u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05D4\u2026"
lastmod: '2024-03-13T22:44:38.672432-06:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05DE\u05E1\u05E4\u05E7\u05EA \u05D0\
  \u05EA \u05DE\u05D5\u05D3\u05D5\u05DC \u05D4-`csv` \u05D4\u05DE\u05D5\u05D1\u05E0\
  \u05D4 \u05DC\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E7\u05D1\u05E6\u05D9 CSV,\
  \ \u05D5\u05D4\u05D5\u05E4\u05DB\u05EA \u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4\
  \ \u05DE\u05D4\u05DD \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\u05D4\
  \u05DD \u05DC\u05E4\u05E9\u05D5\u05D8\u05D4."
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
weight: 37
---

## איך לעשות:
פייתון מספקת את מודול ה-`csv` המובנה לטיפול בקבצי CSV, והופכת את קריאה מהם וכתיבה אליהם לפשוטה. למניפולציות נתונים חזקות ומורכבות יותר, הספרייה `pandas` מתוך צד שלישי היא פופולרית ביותר.

### שימוש במודול `csv`


#### קריאה של קובץ CSV
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*בהנחה ש-`sample.csv` מכיל:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*פלט:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### כתיבה לקובץ CSV
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*יוצר או דורס את `output.csv` עם:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### שימוש ב`pandas` לCSV
`pandas` היא ספרייה עוצמתית למניפולציית נתונים שמפשטת את העבודה עם קבצי CSV בין פורמטים אחרים.

#### התקנת pandas
```shell
pip install pandas
```

#### קריאה של קובץ CSV עם pandas
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*פלט:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### כתיבה לקובץ CSV עם pandas
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*יוצר או דורס את `output_pandas.csv` עם:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
