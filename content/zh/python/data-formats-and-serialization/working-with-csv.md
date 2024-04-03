---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:04.501287-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Python \u63D0\u4F9B\u4E86\u5185\u7F6E\u7684\
  \ `csv` \u6A21\u5757\u6765\u5904\u7406 CSV \u6587\u4EF6\uFF0C\u4F7F\u5F97\u4ECE\u8FD9\
  \u4E9B\u6587\u4EF6\u8BFB\u53D6\u548C\u5199\u5165\u6570\u636E\u53D8\u5F97\u7B80\u5355\
  \u76F4\u63A5\u3002\u5BF9\u4E8E\u66F4\u5065\u58EE\u548C\u590D\u6742\u7684\u6570\u636E\
  \u64CD\u4F5C\uFF0C\u7B2C\u4E09\u65B9\u5E93 `pandas` \u975E\u5E38\u6D41\u884C\u3002\
  \ #."
lastmod: '2024-03-13T22:44:47.281262-06:00'
model: gpt-4-0125-preview
summary: "Python \u63D0\u4F9B\u4E86\u5185\u7F6E\u7684 `csv` \u6A21\u5757\u6765\u5904\
  \u7406 CSV \u6587\u4EF6\uFF0C\u4F7F\u5F97\u4ECE\u8FD9\u4E9B\u6587\u4EF6\u8BFB\u53D6\
  \u548C\u5199\u5165\u6570\u636E\u53D8\u5F97\u7B80\u5355\u76F4\u63A5\u3002\u5BF9\u4E8E\
  \u66F4\u5065\u58EE\u548C\u590D\u6742\u7684\u6570\u636E\u64CD\u4F5C\uFF0C\u7B2C\u4E09\
  \u65B9\u5E93 `pandas` \u975E\u5E38\u6D41\u884C."
title: "\u5904\u7406CSV\u6587\u4EF6"
weight: 37
---

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
