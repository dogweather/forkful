---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:41.312490-07:00
description: "Fish\u2026"
lastmod: '2024-03-13T22:44:42.766157-06:00'
model: gpt-4-0125-preview
summary: "Fish\u2026"
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
