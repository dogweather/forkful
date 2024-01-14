---
title:                "Python: 「CSVの取り扱い」"
simple_title:         "「CSVの取り扱い」"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-csv.md"
---

{{< edit_this_page >}}

# なぜCSVを扱うのか

CSVは、データをテキスト形式で保存するための便利な方法です。データベースや表計算ソフトよりも柔軟で使いやすく、Pythonプログラミングでよく使われています。CSVを扱うことで、データの抽出や分析が簡単になります。

## 手順

まずはCSVファイルを読み込みます。```Python
import csv

with open('data.csv', 'r') as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)
```

これにより、CSVファイルの全ての行が出力されます。また、リストなどのデータ構造に保存することもできます。例えば、次のようなCSVファイルがあるとします。

```Python
product_name, price
Apple, 100
Banana, 200
Orange, 150
```

このファイルを読み込んで、リストに保存するには以下のようにします。

```Python
import csv

products = []

with open('data.csv', 'r') as file:
    reader = csv.reader(file)
    for row in reader:
        products.append(row)

print(products) # [['product_name', 'price'], ['Apple', '100'], ['Banana', '200'], ['Orange', '150']]
```

CSVファイルを書き込む際も同様の方法を使うことができます。例えば、新しい商品を追加したい場合は、以下のようにします。

```Python
import csv

new_product = ['Pineapple', '300']

with open('data.csv', 'a') as file:
    writer = csv.writer(file)
    writer.writerow(new_product)
```

これにより、新しい行がCSVファイルに追加されます。

## 深掘り

CSVファイルを読み込む際に、追加のオプションを使うことでより詳細な操作が可能です。例えば、```delimiter```を指定することで、デフォルトの```','```以外の区切り文字を使用することができます。

また、CSVファイルから特定の列のみ読み込みたい場合は、```csv.DictReader```を使うと、列のヘッダーをキーワードに指定できるようになります。

さらに、CSVファイルを操作する際には、エラー処理も重要です。```try-except```文を使うことで、読み込みや書き込みで起きたエラーを処理することができます。

## その他の参考サイト

- [PythonでCSVファイルの読み込み](https://note.nkmk.me/python-csv-reader/)
- [様々なオプションを使用してCSVファイルを読み込む方法](https://docs.python.org/ja/3/library/csv.html#module-csv)
- [Pythonでのエラー処理の方法](https://note.nkmk.me/python-try-except-else-finally/)