---
title:                "JSONを活用する"
aliases:
- /ja/bash/working-with-json.md
date:                  2024-02-03T19:21:46.614494-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSONを活用する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
