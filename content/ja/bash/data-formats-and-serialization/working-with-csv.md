---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:53.653490-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.404888-06:00'
model: gpt-4-0125-preview
summary: "Bash\u3067CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u30D5\
  \u30A1\u30A4\u30EB\u3092\u6271\u3046\u3053\u3068\u306F\u3001\u30D7\u30EC\u30FC\u30F3\
  \u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u3067\u4FDD\u5B58\u3055\u308C\u305F\u8868\u5F62\
  \u5F0F\u306E\u30C7\u30FC\u30BF\u3092\u51E6\u7406\u3057\u3001\u64CD\u4F5C\u3059\u308B\
  \u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30B3\
  \u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u304B\u3089\u76F4\u63A5\u30C7\u30FC\u30BF\u5909\
  \u63DB\u3001\u5206\u6790\u3001\u7D71\u5408\u30BF\u30B9\u30AF\u306E\u81EA\u52D5\u5316\
  \u3092\u53EF\u80FD\u306B\u3059\u308B\u305F\u3081\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306B\u3068\u3063\u3066\u4E0D\u53EF\u6B20\u3067\u3059\u3002\u3053\u308C\u306B\
  \u3088\u308A\u3001\u3088\u308A\u91CD\u3044\u30C4\u30FC\u30EB\u3084\u30D7\u30ED\u30B0\
  \u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3092\u5FC5\u8981\u3068\u305B\u305A\u306B\u6E08\
  \u307F\u307E\u3059\u3002."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 何となぜ？
BashでCSV（カンマ区切り値）ファイルを扱うことは、プレーンテキスト形式で保存された表形式のデータを処理し、操作することを意味します。これは、コマンドラインから直接データ変換、分析、統合タスクの自動化を可能にするため、プログラマーにとって不可欠です。これにより、より重いツールやプログラミング環境を必要とせずに済みます。

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
