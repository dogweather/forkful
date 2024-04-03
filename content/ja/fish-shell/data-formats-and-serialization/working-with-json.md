---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:41.312490-07:00
description: "Fish\u2026"
lastmod: '2024-03-13T22:44:42.766157-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell\u3067JSON\u3092\u6271\u3046\u3053\u3068\u306F\u3001\u30A2\u30D7\
  \u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u8A2D\u5B9A\u3001API\u3068\u306E\u3084\
  \u308A\u53D6\u308A\u3001\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u30EF\u30FC\u30AF\
  \u30D5\u30ED\u30FC\u306E\u5408\u7406\u5316\u306A\u3069\u3001\u4E00\u822C\u7684\u306A\
  \u30BF\u30B9\u30AF\u306B\u304A\u3044\u3066JSON\u30C7\u30FC\u30BF\u306E\u89E3\u6790\
  \u3068\u751F\u6210\u3092\u542B\u307F\u307E\u3059\u3002\u30A6\u30A7\u30D6\u3068\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u958B\u767A\u3067\u306EJSON\u306E\u666E\
  \u904D\u6027\u3092\u8003\u3048\u308B\u3068\u3001\u30B7\u30A7\u30EB\u5185\u3067\u76F4\
  \u63A5\u305D\u306E\u64CD\u4F5C\u3092\u30DE\u30B9\u30BF\u30FC\u3059\u308B\u3053\u3068\
  \u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306E\u81EA\u52D5\u5316\u3068\u30C7\
  \u30FC\u30BF\u51E6\u7406\u52B9\u7387\u3092\u5927\u5E45\u306B\u9AD8\u3081\u308B\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## 何となぜ？

Fish ShellでJSONを扱うことは、アプリケーションの設定、APIとのやり取り、コマンドラインワークフローの合理化など、一般的なタスクにおいてJSONデータの解析と生成を含みます。ウェブとアプリケーション開発でのJSONの普遍性を考えると、シェル内で直接その操作をマスターすることは、プログラマーの自動化とデータ処理効率を大幅に高めることができます。

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
