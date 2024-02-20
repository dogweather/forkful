---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:04.501287-07:00
description: "\u4F7F\u7528 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6D89\u53CA\
  \u4ECE CSV \u6587\u4EF6\u8BFB\u53D6\u6570\u636E\u548C\u5411 CSV \u6587\u4EF6\u5199\
  \u5165\u6570\u636E\uFF0C\u8FD9\u662F\u4E00\u79CD\u5E38\u89C1\u7684\u5B58\u50A8\u8868\
  \u683C\u6570\u636E\u7684\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u8F7B\u677E\u5730\u5728\u4E0D\u540C\u5E73\u53F0\u548C\u8BED\u8A00\u4E4B\
  \u95F4\u4EA4\u6362\u548C\u5B58\u50A8\u6570\u636E\uFF0C\u4F7F\u7528\u4E00\u79CD\u7B80\
  \u5355\u7684\u57FA\u4E8E\u6587\u672C\u7684\u683C\u5F0F\uFF0C\u5F97\u5230\u4E86\u5E7F\
  \u6CDB\u652F\u6301\u3002"
lastmod: 2024-02-19 22:05:06.361786
model: gpt-4-0125-preview
summary: "\u4F7F\u7528 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6D89\u53CA\u4ECE\
  \ CSV \u6587\u4EF6\u8BFB\u53D6\u6570\u636E\u548C\u5411 CSV \u6587\u4EF6\u5199\u5165\
  \u6570\u636E\uFF0C\u8FD9\u662F\u4E00\u79CD\u5E38\u89C1\u7684\u5B58\u50A8\u8868\u683C\
  \u6570\u636E\u7684\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u8F7B\u677E\u5730\u5728\u4E0D\u540C\u5E73\u53F0\u548C\u8BED\u8A00\u4E4B\u95F4\
  \u4EA4\u6362\u548C\u5B58\u50A8\u6570\u636E\uFF0C\u4F7F\u7528\u4E00\u79CD\u7B80\u5355\
  \u7684\u57FA\u4E8E\u6587\u672C\u7684\u683C\u5F0F\uFF0C\u5F97\u5230\u4E86\u5E7F\u6CDB\
  \u652F\u6301\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
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
