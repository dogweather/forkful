---
title:                "csv ファイルを操作する"
html_title:           "Python: csv ファイルを操作する"
simple_title:         "csv ファイルを操作する"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Why
CSV（Comma-Separated Values）ファイルは、データを簡単に保存し、読み取ることができる形式です。Pythonを使うと、CSVファイルを効率的に処理でき、データ解析の作業がスムーズになります。

## How To
CSVファイルを読み取るには、Pythonの標準ライブラリである「csv」モジュールを使用します。以下のコード例を参考に、CSVファイルを読み取り、データを出力する方法をご紹介します。

```Python
import csv

# CSVファイルの読み取り
with open('data.csv') as csv_file:
    reader = csv.reader(csv_file, delimiter=',')
    # 一行ずつデータを読み取り、リストとして保存
    for row in reader:
        print(row)
```

このコードでは、`data.csv`というファイルを読み取り、CSVファイル内のデータを一行ずつ`row`というリストに保存しています。`print()`を使うことで、データをターミナル上に出力することができます。

もし、CSVファイル内の特定の列だけを抽出したい場合は、以下のようにコードを書くことができます。

```Python
# 列の指定
with open('data.csv') as csv_file:
    reader = csv.reader(csv_file, delimiter=',')
    for row in reader:
        # 2列目のみを抽出
        print(row[1])
```

また、データをCSVファイルに書き込むには、`csv.writer()`を使用します。以下のコード例では、新しいCSVファイルにヘッダー行とデータを書き込んでいます。

```Python
import csv

# データ
data = [
    ['名前', '年齢', '性別'],
    ['太郎', 25, '男性'],
    ['花子', 30, '女性'],
    ['次郎', 22, '男性']
]

# 新しいCSVファイルを作成し、データを書き込む
with open('new_data.csv', 'w') as csv_file:
    writer = csv.writer(csv_file, delimiter=',')
    writer.writerows(data)
```

このコードを実行すると、`new_data.csv`という新しいCSVファイルが作成され、各列のデータが書き込まれます。

## Deep Dive
CSVファイルのデータを取得する方法や書き込む方法だけでなく、`csv`モジュールにはさまざまな機能があります。例えば、列のタイトルを指定してデータを取得したり、ファイルを上書きするか追記するかを選択したりすることができます。詳細な情報は、[公式ドキュメント](https://docs.python.org/3/library/csv.html)を参照してください。

## See Also
- [Pythonの標準ライブラリ：csvモジュール](https://docs.python.org/ja/3/library/csv.html)
- [CSVファイルを読み書きする方法](https://note.nkmk.me/python-pandas-csv-io-read-write/)
- [PythonでCSVファイルを扱う際の注意点](https://qiita.com/niyanchun/items/084faa6393dda568eb39)