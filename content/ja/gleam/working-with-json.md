---
title:                "「jsonを使ったプログラミング」"
html_title:           "Gleam: 「jsonを使ったプログラミング」"
simple_title:         "「jsonを使ったプログラミング」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## なぜJSONを使うのか

JSONは、データを簡潔で読みやすい形式で表現することができるため、プログラマーにとって非常に便利なフォーマットです。また、多くのウェブアプリケーションやモバイルアプリケーションでも使用されているため、JSONを理解して扱うことは非常に重要です。

## JSONの扱い方

JSONを使うには、Gleamの`jsone`モジュールをインポートする必要があります。次のコードは、簡単なJSONオブジェクトを作成し、その内容をコンソールに出力する例です。

```Gleam
import jsone

let user = jsone.encode({
  "name": "John",
  "age": 30,
  "hobbies": ["reading", "hiking", "cooking"]
})

jsone.pretty_print(user)
```

出力結果は以下のようになります。

```
{
  "name": "John",
  "age": 30,
  "hobbies": [
    "reading",
    "hiking",
    "cooking"
  ]
}
```

また、データをJSONからGleamのデータ型に変換することもできます。次の例では、JSONからGleamのレコード型を作成し、その内容をコンソールに出力しています。

```Gleam
import jsone

let user_string = "{ \"name\": \"Alice\", \"age\": 25, \"hobbies\": [\"painting\", \"traveling\"] }"

let user = jsone.decode(user_string)

io.println(user.name) // => "Alice"
io.println(user.age) // => 25
for hobby in user.hobbies {
  jsone.pretty_print(hobby) // => "painting" と "traveling"が順に出力される
}
```

## JSONの深層への旅

より複雑なJSONオブジェクトを扱うためには、Gleamのパターンマッチングを使うことができます。次の例では、JSONの配列から特定の要素を取り出し、それをGleamの整数型に変換しています。

```Gleam
import jsone

let numbers_string = "[1, 2, 3, 4, 5]"

let numbers = jsone.decode(numbers_string)

let first = case numbers {
  [head, .._] -> jsone.int(head)
  _ -> panic("This should not happen!")
}

io.println(first) // => 1
```

## 関連情報

- [Gleam公式ドキュメント（英語）](https://gleam.run/)
- [GleamのJSONモジュールについて（英語）](https://gleam.run/documentation/stdlib/jsone/)