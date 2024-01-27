---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
CSVファイルは「Comma-Separated Values」の略で、テキストデータを表形式で保存するシンプルな形式です。プログラマーはデータ交換や保存のためによくCSVを使います。

## How to: (方法)
```Bash
# CSVファイルの読み込み例
while IFS=, read -r column1 column2 column3
do
  echo "カラム1: $column1 カラム2: $column2 カラム3: $column3"
done < sample.csv
```

出力例:
```
カラム1: データ1 カラム2: データ2 カラム3: データ3
```

```Bash
# CSVファイルに書き込む例
echo "データ1,データ2,データ3" >> another_sample.csv
```

`another_sample.csv`に追加された行:
```
データ1,データ2,データ3
```

## Deep Dive (深掘り)
CSVは1970年代に登場し、データの単純な入出力のための標準的な形式になりました。代替としてXMLやJSONなどがありますが、CSVのシンプルさが依然として魅力です。BashでCSVを扱う際には、フィールド分割や特殊文字のエスケープなどの詳細を考慮する必要があります。

## See Also (関連情報)
- [GNU Awk のユーザーガイド](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Bash スクリプトガイド](https://mywiki.wooledge.org/BashGuide)
- [CSV Wikipedia ページ](https://ja.wikipedia.org/wiki/Comma-Separated_Values)
