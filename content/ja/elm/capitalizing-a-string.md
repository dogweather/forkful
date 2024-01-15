---
title:                "「文字列の大文字化」"
html_title:           "Elm: 「文字列の大文字化」"
simple_title:         "「文字列の大文字化」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

あなたはプログラムで文字列を大文字にする必要があるかもしれません。例えば、ユーザーの入力した文字列をデータベースに保存する場合、大文字と小文字を区別せずに検索するためには、入力された文字列をすべて大文字に変換する必要があります。

## 方法

Elmでは、文字列を大文字に変換するために `String.toUpper` 関数を使用します。以下のコード例を参考にしてください。

```Elm
import String exposing (toUpper)

myString = "Hello, World!"
upperString = toUpper myString
```

上記のコードでは、`toUpper` に `myString` を渡すことで、`Hello, World!`が `HELLO, WORLD!`に変換されます。また、この関数はスペースや記号なども変換するため、あらゆる文字列を大文字に変換することができます。

## ディープダイブ

Elmでは `String.toUpper` 関数以外にも、`String.toLower` 関数を使うことで文字列を小文字に変換することもできます。また、`String.toTitle` 関数を使うことで最初の文字だけを大文字にすることもできます。さらに、文字列の一部を大文字や小文字に変換する `String.toUpperAt` や `String.toLowerAt` 関数も存在します。

## その他の参考文献

- [Elm公式ドキュメント](https://guide.elm-lang.org/)
- [Elm公式フォーラム](https://elm-lang.org/community)
- [入門 Elm - モダンWeb開発デザインのなかのElm](https://www.oreilly.co.jp/books/9784873117922/)