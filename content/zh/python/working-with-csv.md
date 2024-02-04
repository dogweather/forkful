---
title:                "处理CSV文件"
date:                  2024-02-03T19:21:04.501287-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理CSV文件"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?
使用 CSV（逗号分隔值）涉及从 CSV 文件读取数据和向 CSV 文件写入数据，这是一种常见的存储表格数据的格式。程序员这样做是为了轻松地在不同平台和语言之间交换和存储数据，使用一种简单的基于文本的格式，得到了广泛支持。

## 如何操作:
Python 提供了内置的 `csv` 模块来处理 CSV 文件，使得从这些文件读取和写入数据变得简单直接。对于更健壮和复杂的数据操作，第三方库 `pandas` 非常流行。

### 使用 `csv` 模块

#### 读取 CSV 文件
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*假设 `sample.csv` 包含:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*输出:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### 写入 CSV 文件
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*创建或覆盖 `output.csv`，内容为:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### 使用 `pandas` 进行 CSV 操作
`pandas` 是一个强大的数据操作库，简化了 CSV 文件等数据格式的处理。

#### 安装 pandas
```shell
pip install pandas
```

#### 使用 pandas 读取 CSV 文件
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*输出:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### 使用 pandas 写入 CSV 文件
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*创建或覆盖 `output_pandas.csv`，内容为:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
