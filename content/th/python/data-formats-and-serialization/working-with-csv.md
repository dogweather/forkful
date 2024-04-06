---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:30.252705-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Python \u0E43\u0E2B\u0E49\
  \u0E42\u0E21\u0E14\u0E39\u0E25\u0E43\u0E19\u0E15\u0E31\u0E27 `csv` \u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E44\u0E1F\
  \u0E25\u0E4C CSV \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\
  \u0E19\u0E08\u0E32\u0E01\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E25\u0E07\
  \u0E44\u0E1B\u0E07\u0E48\u0E32\u0E22 \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\
  \u0E35\u0E48\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19\u0E41\u0E25\u0E30\u0E41\u0E02\
  \u0E47\u0E07\u0E41\u0E01\u0E23\u0E48\u0E07\u0E21\u0E32\u0E01\u0E02\u0E36\u0E49\u0E19\
  \u2026"
lastmod: '2024-04-05T21:54:01.208319-06:00'
model: gpt-4-0125-preview
summary: "Python \u0E43\u0E2B\u0E49\u0E42\u0E21\u0E14\u0E39\u0E25\u0E43\u0E19\u0E15\
  \u0E31\u0E27 `csv` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E01\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C CSV \u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E08\u0E32\u0E01\u0E41\u0E25\u0E30\u0E40\
  \u0E02\u0E35\u0E22\u0E19\u0E25\u0E07\u0E44\u0E1B\u0E07\u0E48\u0E32\u0E22 \u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\
  \u0E19\u0E41\u0E25\u0E30\u0E41\u0E02\u0E47\u0E07\u0E41\u0E01\u0E23\u0E48\u0E07\u0E21\
  \u0E32\u0E01\u0E02\u0E36\u0E49\u0E19 \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E02\
  \u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E2D\
  \u0E22\u0E48\u0E32\u0E07 `pandas` \u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E04\u0E27\
  \u0E32\u0E21\u0E19\u0E34\u0E22\u0E21\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E2A\u0E39\u0E07\
  ."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## วิธีการ:
Python ให้โมดูลในตัว `csv` สำหรับจัดการกับไฟล์ CSV ทำให้การอ่านจากและเขียนลงไปง่าย สำหรับการจัดการข้อมูลที่ซับซ้อนและแข็งแกร่งมากขึ้น ไลบรารีของบุคคลที่สามอย่าง `pandas` ได้รับความนิยมอย่างสูง

### การใช้งานโมดูล `csv`


#### การอ่านไฟล์ CSV
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*สมมติว่า `sample.csv` มีข้อมูลดังนี้:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*ผลลัพธ์:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### การเขียนไฟล์ CSV
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*สร้างหรือเขียนทับ `output.csv` ด้วย:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### การใช้ `pandas` สำหรับ CSV
`pandas` เป็นไลบรารีทรงพลังสำหรับการจัดการข้อมูลที่ทำให้การทำงานกับไฟล์ CSV และรูปแบบข้อมูลอื่นๆ ง่ายขึ้น

#### ติดตั้ง pandas
```shell
pip install pandas
```

#### การอ่านไฟล์ CSV ด้วย pandas
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*ผลลัพธ์:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### การเขียนไฟล์ CSV ด้วย pandas
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*สร้างหรือเขียนทับ `output_pandas.csv` ด้วย:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
