---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:46.614494-07:00
description: "Bash\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u3067\u306EJSON\u306E\
  \u53D6\u308A\u6271\u3044\u306B\u306F\u3001\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\
  \u304B\u3089\u76F4\u63A5JSON\u30C7\u30FC\u30BF\u3092\u89E3\u6790\u3001\u62BD\u51FA\
  \u3001\u304A\u3088\u3073\u64CD\u4F5C\u3059\u308B\u3053\u3068\u304C\u542B\u307E\u308C\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B7\u30A7\u30EB\
  \u30B9\u30AF\u30EA\u30D7\u30C8\u3092Web\u2026"
lastmod: '2024-03-13T22:44:42.403925-06:00'
model: gpt-4-0125-preview
summary: "Bash\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u3067\u306EJSON\u306E\u53D6\
  \u308A\u6271\u3044\u306B\u306F\u3001\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u304B\
  \u3089\u76F4\u63A5JSON\u30C7\u30FC\u30BF\u3092\u89E3\u6790\u3001\u62BD\u51FA\u3001\
  \u304A\u3088\u3073\u64CD\u4F5C\u3059\u308B\u3053\u3068\u304C\u542B\u307E\u308C\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B7\u30A7\u30EB\u30B9\
  \u30AF\u30EA\u30D7\u30C8\u3092Web\u2026"
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## 何となぜ？
BashプログラミングでのJSONの取り扱いには、コマンドラインから直接JSONデータを解析、抽出、および操作することが含まれます。プログラマーは、シェルスクリプトをWeb APIや現代のデータ交換フォーマットとシームレスに統合するためにこれを行うことが多く、BashスクリプティングをJSONを多用するエコシステムでより強力かつ関連性のあるものにします。

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
