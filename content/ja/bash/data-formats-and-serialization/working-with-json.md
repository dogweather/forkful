---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:46.614494-07:00
description: "\u4F7F\u3044\u65B9: Bash\u81EA\u4F53\u306B\u306F\u7D44\u307F\u8FBC\u307F\
  \u306EJSON\u89E3\u6790\u6A5F\u80FD\u304C\u3042\u308A\u307E\u305B\u3093\u304C\u3001\
  `jq`\u306F\u3053\u306E\u30AE\u30E3\u30C3\u30D7\u3092\u57CB\u3081\u308B\u5F37\u529B\
  \u306A\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3JSON\u30D7\u30ED\u30BB\u30C3\u30B5\
  \u3067\u3059\u3002\u4F7F\u3044\u65B9\u306F\u4EE5\u4E0B\u306E\u901A\u308A\u3067\u3059\
  : **JSON\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F:** `data.json`\u306E\
  \u30B5\u30F3\u30D7\u30EB."
lastmod: '2024-03-13T22:44:42.403925-06:00'
model: gpt-4-0125-preview
summary: "Bash\u81EA\u4F53\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306EJSON\u89E3\u6790\
  \u6A5F\u80FD\u304C\u3042\u308A\u307E\u305B\u3093\u304C\u3001`jq`\u306F\u3053\u306E\
  \u30AE\u30E3\u30C3\u30D7\u3092\u57CB\u3081\u308B\u5F37\u529B\u306A\u30B3\u30DE\u30F3\
  \u30C9\u30E9\u30A4\u30F3JSON\u30D7\u30ED\u30BB\u30C3\u30B5\u3067\u3059\u3002\u4F7F\
  \u3044\u65B9\u306F\u4EE5\u4E0B\u306E\u901A\u308A\u3067\u3059."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## 使い方:
Bash自体には組み込みのJSON解析機能がありませんが、`jq`はこのギャップを埋める強力なコマンドラインJSONプロセッサです。使い方は以下の通りです:

**JSONファイルの読み込み:**

`data.json`のサンプル:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

JSONファイルから名前を読み取り抽出するには:
```bash
jq '.name' data.json
```
出力:
```
"Jane Doe"
```

**JSONデータの修正:**

都市を「Los Angeles」に更新してファイルに書き戻すには:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**変数からのJSON解析:**

Bash変数にJSONが含まれている場合、`jq`はそれでも処理できます:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
出力:
```
"John Doe"
```

**配列の扱い:**

JSON内の項目の配列が与えられた場合:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

2番目のアイテムを抽出するには（インデックスは0から開始）:
```bash
jq '.items[1]' data.json
```
出力:
```
"banana"
```

より複雑な操作やフィルタリングについては、`jq`には包括的なマニュアルとチュートリアルがオンラインで利用可能であり、すべてのBash/JSONのニーズに対する多用途なツールとなっています。
