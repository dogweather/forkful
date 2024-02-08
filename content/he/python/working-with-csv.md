---
title:                "עובדים עם CSV"
aliases:
- he/python/working-with-csv.md
date:                  2024-02-03T19:21:13.744336-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם CSV (ערכים מופרדים בפסיק) כוללת קריאה מקבצי CSV וכתיבה אליהם, פורמט נפוץ לאחסון נתונים טבלאיים. מתכנתים עושים זאת כדי להחליף ולאחסן נתונים בקלות בפורמט טקסטואלי פשוט, התומך רחבות בפלטפורמות ושפות שונות.

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
