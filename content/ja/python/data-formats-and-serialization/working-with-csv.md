---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:27.233920-07:00
description: "\u65B9\u6CD5 Python\u306FCSV\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\
  \u305F\u3081\u306B\u7D44\u307F\u8FBC\u307F\u306E`csv`\u30E2\u30B8\u30E5\u30FC\u30EB\
  \u3092\u63D0\u4F9B\u3057\u3001\u3053\u308C\u3092\u4F7F\u3063\u3066\u304B\u3089\u306E\
  \u8AAD\u307F\u53D6\u308A\u3068\u66F8\u304D\u8FBC\u307F\u304C\u7C21\u5358\u306B\u306A\
  \u308A\u307E\u3059\u3002\u3088\u308A\u5805\u7262\u3067\u8907\u96D1\u306A\u30C7\u30FC\
  \u30BF\u64CD\u4F5C\u306B\u306F\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\
  \u30E9\u30A4\u30D6\u30E9\u30EA`pandas`\u304C\u975E\u5E38\u306B\u4EBA\u6C17\u3067\
  \u3059\u3002"
lastmod: '2024-04-05T21:53:42.481282-06:00'
model: gpt-4-0125-preview
summary: ''
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 方法
PythonはCSVファイルを扱うために組み込みの`csv`モジュールを提供し、これを使ってからの読み取りと書き込みが簡単になります。より堅牢で複雑なデータ操作には、サードパーティのライブラリ`pandas`が非常に人気です。

### `csv`モジュールを使用する


#### CSVファイルを読む
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*`sample.csv`が以下を含むと仮定します：*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*出力：*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### CSVファイルに書き込む
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*`output.csv`を以下で作成または上書きします：*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### CSVに対して`pandas`を使用する
`pandas`はデータ操作を簡単にする強力なライブラリで、CSVファイルなどのさまざまなデータ形式を扱うことを簡略化します。

#### pandasをインストールする
```shell
pip install pandas
```

#### pandasでCSVファイルを読む
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*出力：*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### pandasでCSVファイルに書き込む
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*`output_pandas.csv`を以下で作成または上書きします：*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
