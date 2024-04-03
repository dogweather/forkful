---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:53.653490-07:00
description: "\u65B9\u6CD5: **CSV\u30D5\u30A1\u30A4\u30EB\u3092\u884C\u3054\u3068\u306B\
  \u8AAD\u307F\u8FBC\u3080**."
lastmod: '2024-03-13T22:44:42.404888-06:00'
model: gpt-4-0125-preview
summary: "**CSV\u30D5\u30A1\u30A4\u30EB\u3092\u884C\u3054\u3068\u306B\u8AAD\u307F\u8FBC\
  \u3080**."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 方法:
**CSVファイルを行ごとに読み込む**

```bash
while IFS=, read -r column1 column2 column3
do
  echo "列1: $column1, 列2: $column2, 列3: $column3"
done < sample.csv
```

*サンプル出力:*

```
列1: id, 列2: name, 列3: email
...
```

**条件に基づいてCSVの行をフィルタリングする**

`awk`を使用すると、簡単に行をフィルタリングできます。たとえば、2列目が「Alice」である行を探すには:

```bash
awk -F, '$2 == "Alice" { print $0 }' sample.csv
```

**列の値を変更する**

2列目を大文字に変更するには:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' sample.csv
```

**列に基づいてCSVファイルをソートする**

例えば、3番目の列（数値順）に基づいてCSVファイルをソートできます:

```bash
sort -t, -k3,3n sample.csv
```

**より複雑なタスクのための `csvkit` の使用**

`csvkit`は、CSVに変換して作業するためのコマンドラインツールのスイートです。pipを通じてインストールできます。

JSONファイルをCSVに変換するには:

```bash
in2csv data.json > data.csv
```

SQLを使用してCSVファイルをクエリするには:

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" sample.csv
```

*注意: `csvkit`のインストールにはPythonが必要で、`pip install csvkit`を使用して行います。*
