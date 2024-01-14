---
title:                "Gleam: 「JSONを使用する」"
simple_title:         "「JSONを使用する」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/working-with-json.md"
---

{{< edit_this_page >}}

こんにちは、日本の読者の皆さん！今日はGleamプログラミングについてお話ししたいと思います。Gleamは最近話題のプログラミング言語ですが、その中でもJSONとの連携がとても強力です。では、なぜJSONを扱うことが重要なのか、どうやってコードを書くのか、そしてJSONについて深く掘り下げてみましょう。

## なぜ？

JSONは、webアプリケーションやAPIなどでよく使われるデータ形式です。そのため、JSONを理解し、処理することはとても重要です。Gleamでは、便利な関数やモジュールを使うことで、簡単にJSONを扱うことができます。

## 使い方

GleamでJSONを扱うには、```gleam/json```モジュールを使います。例えば、以下のようにJSONを作成することができます。

```Gleam
import gleam/json

let data = json.object([
  ("name", json.string("John")),
  ("age", json.int(30))
])

```

また、JSONをパースするには、```gleam/json/parse```関数を使います。例えば、以下のようにパースした結果を表示することができます。

```Gleam
import gleam/json/parse

let data = """{"name": "John", "age": 30}"""

let parsed = case parse(data) {
  Ok(data) -> data
  Err(_) -> json.object([])
}

debug(parsed["name"]) // "John"
```

## ディープダイブ

Gleamでは、配列やネストされたデータなど、さまざまな形式のJSONを処理することができます。また、パターンマッチングを使うことで、より高度な処理を行うことも可能です。詳しくは、[公式ドキュメント](https://gleam.run/)を参考にしてください。

## はてな

最後に、JSONを扱う上で便利なモジュールやライブラリを紹介します。

- [gleam/json](https://github.com/gleam-lang/json) - GleamでJSONを扱うための標準ライブラリ
- [gleam/http](https://github.com/gleam-lang/http) - HTTPリクエストを処理するためのライブラリ
- [gleam/decode](https://github.com/gleam-lang/decode) - JSONをより柔軟に扱うためのライブラリ

## 参考リンク

- [Gleam公式ドキュメント](https://gleam.run/)
- [JSONの基本を理解する](https://www.json.org/json-ja.html)
- [JSONの仕様書](https://tools.ietf.org/html/rfc8259)