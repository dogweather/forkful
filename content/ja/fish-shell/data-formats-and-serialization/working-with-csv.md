---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:48.953111-07:00
description: "\u65B9\u6CD5\uFF1A Fish Shell\u81EA\u4F53\u306F\u3001CSV\u64CD\u4F5C\
  \u306E\u305F\u3081\u306B\u7279\u5225\u306B\u8A2D\u8A08\u3055\u308C\u305F\u7D44\u307F\
  \u8FBC\u307F\u6A5F\u80FD\u3092\u6301\u3063\u3066\u3044\u307E\u305B\u3093\u3002\u3057\
  \u304B\u3057\u3001`awk`\u3001`sed`\u3001`cut`\u306E\u3088\u3046\u306AUnix\u30E6\u30FC\
  \u30C6\u30A3\u30EA\u30C6\u30A3\u3092\u57FA\u672C\u64CD\u4F5C\u306B\u6D3B\u7528\u3057\
  \u305F\u308A\u3001\u3088\u308A\u9AD8\u5EA6\u306A\u30BF\u30B9\u30AF\u306B\u306F`csvkit`\u306E\
  \u3088\u3046\u306A\u5C02\u9580\u7684\u306A\u30C4\u30FC\u30EB\u3092\u4F7F\u7528\u3059\
  \u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002 #."
lastmod: '2024-03-13T22:44:42.767700-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell\u81EA\u4F53\u306F\u3001CSV\u64CD\u4F5C\u306E\u305F\u3081\u306B\
  \u7279\u5225\u306B\u8A2D\u8A08\u3055\u308C\u305F\u7D44\u307F\u8FBC\u307F\u6A5F\u80FD\
  \u3092\u6301\u3063\u3066\u3044\u307E\u305B\u3093\u3002\u3057\u304B\u3057\u3001`awk`\u3001\
  `sed`\u3001`cut`\u306E\u3088\u3046\u306AUnix\u30E6\u30FC\u30C6\u30A3\u30EA\u30C6\
  \u30A3\u3092\u57FA\u672C\u64CD\u4F5C\u306B\u6D3B\u7528\u3057\u305F\u308A\u3001\u3088\
  \u308A\u9AD8\u5EA6\u306A\u30BF\u30B9\u30AF\u306B\u306F`csvkit`\u306E\u3088\u3046\
  \u306A\u5C02\u9580\u7684\u306A\u30C4\u30FC\u30EB\u3092\u4F7F\u7528\u3059\u308B\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059."
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
