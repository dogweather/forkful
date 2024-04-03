---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:48.953111-07:00
description: "CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u30D5\u30A1\
  \u30A4\u30EB\u3092\u6271\u3046\u3053\u3068\u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u306B\u5E83\u304F\u4F7F\
  \u308F\u308C\u308B\u8868\u5F62\u5F0F\u306E\u30C7\u30FC\u30BF\u3092\u30D1\u30FC\u30B9\
  \uFF08\u89E3\u6790\uFF09\u3001\u64CD\u4F5C\u3001\u304A\u3088\u3073\u751F\u6210\u3059\
  \u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30C7\u30FC\u30BF\u3092\u52B9\u7387\u7684\u306B\u51E6\u7406\u3057\
  \u5206\u6790\u3059\u308B\u305F\u3081\u3001\u30BF\u30B9\u30AF\u3092\u81EA\u52D5\u5316\
  \u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\u4ED6\u306E\u30B7\u30B9\u30C6\u30E0\
  \u3068\u7D71\u5408\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3089\u306E\u64CD\u4F5C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.767700-06:00'
model: gpt-4-0125-preview
summary: "CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u30D5\u30A1\u30A4\
  \u30EB\u3092\u6271\u3046\u3053\u3068\u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u306B\u5E83\u304F\u4F7F\u308F\
  \u308C\u308B\u8868\u5F62\u5F0F\u306E\u30C7\u30FC\u30BF\u3092\u30D1\u30FC\u30B9\uFF08\
  \u89E3\u6790\uFF09\u3001\u64CD\u4F5C\u3001\u304A\u3088\u3073\u751F\u6210\u3059\u308B\
  \u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u30C7\u30FC\u30BF\u3092\u52B9\u7387\u7684\u306B\u51E6\u7406\u3057\u5206\
  \u6790\u3059\u308B\u305F\u3081\u3001\u30BF\u30B9\u30AF\u3092\u81EA\u52D5\u5316\u3059\
  \u308B\u305F\u3081\u3001\u307E\u305F\u306F\u4ED6\u306E\u30B7\u30B9\u30C6\u30E0\u3068\
  \u7D71\u5408\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3089\u306E\u64CD\u4F5C\u3092\
  \u884C\u3044\u307E\u3059\u3002."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 方法：
Fish Shell自体は、CSV操作のために特別に設計された組み込み機能を持っていません。しかし、`awk`、`sed`、`cut`のようなUnixユーティリティを基本操作に活用したり、より高度なタスクには`csvkit`のような専門的なツールを使用することができます。

### CSVファイルを読んで最初の列を印刷する：
最初の列を抽出するために`cut`を使う：
```fish
cut -d ',' -f1 data.csv
```
サンプル出力：
```
Name
Alice
Bob
```

### 列の値に基づいてCSV行をフィルタリングする：
第2列が"42"と一致する行を見つけるために`awk`を使う：
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
サンプル出力：
```
Bob,42,London
```

### CSVファイルを変更する（例えば、列を追加）：
静的な値"NewColumn"がある列を追加するために`awk`を使用する：
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"NewColumn"}' data.csv > modified.csv
```
`modified.csv`のサンプル出力：
```
Name,Age,City,NewColumn
Alice,30,New York,NewColumn
Bob,42,London,NewColumn
```

### より高度な操作のために`csvkit`を使用する：
まず、`csvkit`がインストールされていることを確認してください。そうでない場合はpipを使ってインストールします：`pip install csvkit`。

**CSVファイルをJSONに変換する：**
```fish
csvjson data.csv > data.json
```
`data.json`のサンプル出力：
```json
[{"Name":"Alice","Age":"30","City":"New York"},{"Name":"Bob","Age":"42","City":"London"}]
```

**`csvkit`の`csvgrep`でフィルタリングする：**
```fish
csvgrep -c 2 -m 42 data.csv
```
このコマンドはフィルタリングタスクを再現し、`csvkit`を使用して列2の値"42"を対象とします。

結論として、Fish Shell自体は直接的なCSV操作機能を提供しないかもしれませんが、Unixユーティリティと`csvkit`のようなツールの利用可能性とのシームレスな統合により、CSVファイルを扱うための強力なオプションを提供します。
