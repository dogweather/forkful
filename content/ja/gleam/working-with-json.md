---
title:                "JSONを扱う方法"
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
JSONはデータ交換のフォーマットです。簡単で、軽量、プログラム間のデータ移動に使う。速くて、楽だからプログラマーは好んで使います。

## How to: (やり方)
```gleam
import gleam/json
import gleam/map

pub fn main() {
  let data = map.from_list([
    ("name", "Taro"),
    ("age", 30),
  ])
  let json = json.from_map(data)
  let string = json.to_string()

  string // "{"name":"Taro","age":30}"
}
```

出力:
```
{"name":"Taro","age":30}
```

エラーハンドリング:
```gleam
import gleam/json.{DecodeError}

pub fn decode_age(json_string: String) -> Result(Int, DecodeError) {
  json_string
  |> json.decode_string
  |> result.map(json.get_field("age"))
  |> result.map(json.to_int)
}

decode_age("{\"age\":30}") // Ok(30)
decode_age("{\"age\":\"thirty\"}") // Error(DecodeError)
```

## Deep Dive (深堀り)
JSON (JavaScript Object Notation) は2000年代初頭に登場しました。軽量なため、XMLの代わりとして人気に。Gleamでは`gleam/json`ライブラリで扱う。パフォーマンスとパターンマッチング強化が主な実装の動機。

## See Also (関連情報)
- JSON公式ウェブサイト: [JSON.org](https://www.json.org/json-en.html)
- 他のデータ交換フォーマット: [YAML](https://yaml.org/), [XML](https://www.w3.org/XML/)