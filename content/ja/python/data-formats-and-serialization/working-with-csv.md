---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:27.233920-07:00
description: "CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u3092\u6271\
  \u3046\u3053\u3068\u306F\u3001CSV\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u8AAD\u307F\
  \u53D6\u308A\u3001\u66F8\u304D\u8FBC\u307F\u3092\u884C\u3046\u3053\u3068\u3092\u542B\
  \u307F\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u8868\u5F62\u5F0F\u306E\u30C7\u30FC\
  \u30BF\u3092\u4FDD\u5B58\u3059\u308B\u305F\u3081\u306E\u4E00\u822C\u7684\u306A\u5F62\
  \u5F0F\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u7570\u306A\
  \u308B\u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u3084\u8A00\u8A9E\u9593\u3067\
  \u7C21\u5358\u306B\u30C7\u30FC\u30BF\u3092\u4EA4\u63DB\u3057\u3001\u4FDD\u5B58\u3059\
  \u308B\u305F\u3081\u306B\u3001\u30B7\u30F3\u30D7\u30EB\u3067\u30C6\u30AD\u30B9\u30C8\
  \u30D9\u30FC\u30B9\u306E\u5F62\u5F0F\u3092\u4F7F\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.529863-06:00'
model: gpt-4-0125-preview
summary: "CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u3092\u6271\u3046\
  \u3053\u3068\u306F\u3001CSV\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u8AAD\u307F\u53D6\
  \u308A\u3001\u66F8\u304D\u8FBC\u307F\u3092\u884C\u3046\u3053\u3068\u3092\u542B\u307F\
  \u307E\u3059\u3002\u3053\u308C\u306F\u3001\u8868\u5F62\u5F0F\u306E\u30C7\u30FC\u30BF\
  \u3092\u4FDD\u5B58\u3059\u308B\u305F\u3081\u306E\u4E00\u822C\u7684\u306A\u5F62\u5F0F\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u7570\u306A\u308B\
  \u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u3084\u8A00\u8A9E\u9593\u3067\u7C21\
  \u5358\u306B\u30C7\u30FC\u30BF\u3092\u4EA4\u63DB\u3057\u3001\u4FDD\u5B58\u3059\u308B\
  \u305F\u3081\u306B\u3001\u30B7\u30F3\u30D7\u30EB\u3067\u30C6\u30AD\u30B9\u30C8\u30D9\
  \u30FC\u30B9\u306E\u5F62\u5F0F\u3092\u4F7F\u3044\u307E\u3059\u3002"
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 何となく理由
CSV（カンマ区切り値）を扱うことは、CSVファイルから読み取り、書き込みを行うことを含みます。これは、表形式のデータを保存するための一般的な形式です。プログラマーは、異なるプラットフォームや言語間で簡単にデータを交換し、保存するために、シンプルでテキストベースの形式を使います。

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
