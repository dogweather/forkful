---
title:                "Gleam: 文字列を大文字化する"
simple_title:         "文字列を大文字化する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字に変換する理由は多々あります。例えば、データベースから取得したデータを正規化するためや、入力された文字列を一貫性のある形式に変換するためなどが挙げられます。Gleamを使えば、簡単に文字列を大文字に変換することができます。

## 方法

まずは```String.capitalize()```を使用してみましょう。以下のコードを実行すると、指定した文字列が大文字に変換されます。

```Gleam
let string = "my string"
let capitalized = String.capitalize(string)

IO.println(capitalized)
```

出力:

```
My string
```

また、文字列の先頭以外の文字を大文字に変換することもできます。例えば、以下のコードを実行すると、「my string」の2番目の文字以降が大文字に変換されます。

```Gleam
let string = "my string"
let capitalized = String.capitalize(string, 1)

IO.println(capitalized)
```

出力:

```
mY STRING
```

さらに、特定の文字を基準にして大文字に変換することも可能です。例えば、以下のコードを実行すると、「my string」の「s」以降の文字が大文字に変換されます。

```Gleam
let string = "my string"
let capitalized = String.capitalize(string, 4)

IO.println(capitalized)
```

出力:

```
my STRing
```

## 深堀り

実際の文字列の大文字化の仕組みを見てみましょう。```String.capitalize()```関数は、与えられた文字列を新たに作成し、大文字に変換した後に返します。その際に、Unicode文字列を処理するための機能が使用されます。これにより、特殊な文字や絵文字なども正しく大文字化することができます。

## 参考リンク

- Gleamドキュメント: https://gleam.run/documentation/string#capitalize
- Unicode文字列についての詳細: https://unicode.org/