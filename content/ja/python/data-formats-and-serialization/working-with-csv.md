---
title:                "CSVとの作業"
aliases:
- /ja/python/working-with-csv.md
date:                  2024-02-03T19:21:27.233920-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSVとの作業"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
