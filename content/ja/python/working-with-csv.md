---
title:                "「csvとの作業」"
html_title:           "Python: 「csvとの作業」"
simple_title:         "「csvとの作業」"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-csv.md"
---

{{< edit_this_page >}}

## 概要

CSV（Comma Separated Values）とは、コンマで区切られたテキストファイルの形式です。プログラマーがCSVを操作する理由は、データを扱いやすくするためです。これは、スプレッドシートやデータベースなどの多様なデータ形式に変換することができます。

## 方法：

CSVファイルを操作する方法はいくつかありますが、Pythonを使うことで簡単にデータを読み書きすることができます。以下は、CSVファイルを読み込み、データを表示する例です。

```Python
import csv

with open('data.csv', 'r') as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)
```

実行結果は以下のようになります。

```
['宮崎', '佐藤']
['東京', '山田']
['大阪', '鈴木']
```

データを加工して新しいCSVファイルに書き込むことも可能です。以下は、CSVファイルに新しい行を追加する例です。

```Python
import csv

with open('data.csv', 'r') as file:
    reader = csv.reader(file)
    data = list(reader)

data.append(['福岡', '伊藤'])

with open('new_data.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(data)
```

実行結果は、`new_data.csv`ファイルに以下のように書き込まれます。

```
宮崎,佐藤
東京,山田
大阪,鈴木
福岡,伊藤
```

## 詳細

CSVファイルの形式は、1972年にデータ処理用のドキュメントとして提案されました。以来、広く使われているデータ形式の一つです。Python以外にも、ExcelやGoogleシートなどのアプリケーションでもCSVファイルを扱うことができます。

CSVファイルはコンマでデータを区切るだけではなく、タブやセミコロンなどの区切り文字を使うこともできます。また、テキストファイルなので簡単に編集も可能です。

## 関連リンク

- [Python公式ドキュメント - CSVモジュール](https://docs.python.org/ja/3/library/csv.html)
- [CSVファイルの基礎知識](https://qiita.com/tomomoto/items/947dcadccdf3a95227b7)
- [CSVファイルのフォーマット詳細](https://tools.ietf.org/html/rfc4180)