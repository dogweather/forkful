---
title:                "CSVファイルの操作"
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV（Comma-Separated Values）は、テキストデータをシンプルな形式で保存するのに使います。データを表形式で簡単に交換・加工するため、プログラマーはCSVをよく使います。

## How to:
### CSV読み込み:
```Python
import csv

# CSVファイルを開いて内容を表示
with open('example.csv', mode='r', encoding='utf-8') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)

# 出力: ['列1', '列2', '列3'], ['データ1', 'データ2', 'データ3'], ...
```

### CSV書き込み:
```Python
import csv

# CSVファイルにデータを書き込む
data = [['列1', '列2', '列3'], ['データ1', 'データ2', 'データ3']]
with open('output.csv', mode='w', newline='', encoding='utf-8') as file:
    csv_writer = csv.writer(file)
    csv_writer.writerows(data)

# 出力: ファイル`output.csv`にデータが保存される。
```

## Deep Dive
CSVはRFC 4180でフォーマットが定義されています。JSONやXMLのようなデータ交換フォーマットと比べると、CSVは単純で読み書きが早いです。しかし、データの型がないため、数値や日付なども全て文字列として扱います。Pythonの標準ライブラリ`csv`は基本的なCSV操作をサポートしていますが、より高度な機能が必要な場合は`pandas`ライブラリが有用です。

## See Also
- [Pythonの公式ドキュメント（csvモジュール）](https://docs.python.org/3/library/csv.html)
- [Pandasライブラリのドキュメント](https://pandas.pydata.org/pandas-docs/stable/)
- [RFC 4180 - Common Format and MIME Type for Comma-Separated Values (CSV) Files](https://tools.ietf.org/html/rfc4180)
