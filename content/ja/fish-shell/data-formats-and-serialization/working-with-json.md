---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:41.312490-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Fish\u2026"
lastmod: '2024-03-13T22:44:42.766157-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell\u81EA\u4F53\u306B\u306F\u3001JSON\u3092\u89E3\u6790\u304A\u3088\
  \u3073\u751F\u6210\u3059\u308B\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u30E6\u30FC\
  \u30C6\u30A3\u30EA\u30C6\u30A3\u306F\u3042\u308A\u307E\u305B\u3093\u3002\u3057\u304B\
  \u3057\u3001`jq`\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\
  \u88FD\u306E\u30C4\u30FC\u30EB\u3068\u30B7\u30FC\u30E0\u30EC\u30B9\u306B\u7D71\u5408\
  \u3055\u308C\u3001JSON\u51E6\u7406\u304C\u53EF\u80FD\u3067\u3059\u3002`jq`\u306F\
  \u5F37\u529B\u3067\u591A\u6A5F\u80FD\u306A\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\
  JSON\u30D7\u30ED\u30BB\u30C3\u30B5\u3067\u3042\u308A\u3001\u7C21\u5358\u304B\u3064\
  \u8868\u73FE\u529B\u8C4A\u304B\u306A\u8A00\u8A9E\u3067\u69CB\u9020\u5316\u30C7\u30FC\
  \u30BF\u3092\u30B9\u30E9\u30A4\u30B9\u3001\u30D5\u30A3\u30EB\u30BF\u3001\u30DE\u30C3\
  \u30D7\u3001\u5909\u63DB\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## どのようにして：
Fish Shell自体には、JSONを解析および生成するための組み込みユーティリティはありません。しかし、`jq`のようなサードパーティ製のツールとシームレスに統合され、JSON処理が可能です。`jq`は強力で多機能なコマンドラインJSONプロセッサであり、簡単かつ表現力豊かな言語で構造化データをスライス、フィルタ、マップ、変換することができます。

### jqを使ってJSONを解析する
`jq`を使ってJSONファイルを解析し、データを抽出するには：

```fish
# 'data.json'という名前のJSONファイルがあり、その内容が {"name":"Fish Shell","version":"3.4.0"} だと仮定します
cat data.json | jq '.name'
# サンプル出力
"Fish Shell"
```

### jqを使ってJSONを生成する
シェル変数や出力からJSONコンテンツを作成する：

```fish
# 変数からJSONオブジェクトを作成する
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# サンプル出力
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### JSONコレクションをフィルタリングする
`versions.json`というファイルにオブジェクトのJSON配列があるとします：
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
この配列から安定版のみをフィルタリングするには：

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# サンプル出力
"3.1.2"
"3.4.0"
```

提供された例は、Fish Shellでの`jq`の統合によるJSON操作の力を示しています。このようなツールを活用することで、現代のデータフォーマットを扱うための強力な環境として、シェル体験が豊かになります。
