---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:41.312490-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A `jq`\u3092\u4F7F\u3063\
  \u3066JSON\u30D5\u30A1\u30A4\u30EB\u3092\u89E3\u6790\u3057\u3001\u30C7\u30FC\u30BF\
  \u3092\u62BD\u51FA\u3059\u308B\u306B\u306F\uFF1A."
lastmod: '2024-04-05T21:53:43.547020-06:00'
model: gpt-4-0125-preview
summary: ''
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
