---
title:                "文字列の長さを求める"
html_title:           "Elm: 文字列の長さを求める"
simple_title:         "文字列の長さを求める"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを見つけることは、マニュアルのようなものです。これは、文字列を扱う上で基本的な操作であり、プログラムをより効率的に作成するために重要です。

## 作り方

```Elm
stringLength : String -> Int
stringLength str =
    String.length str

main =
    stringLength "こんにちは" -- output: 5
    stringLength "Hello World" -- output: 11
```

ここでは、`String.length`関数を使用して、指定された文字列の長さを見つける方法を示しました。 Elmでは、文字列に関連する便利な関数が多数用意されているため、このような基本的な操作も簡単に実行できます。

## 深堀り

実は、文字列の長さを見つけるには、単純に文字列の長さをカウントするだけでなく、Unicodeの処理も考慮する必要があります。例えば、日本語の文字は通常、1文字を表すのにUTF-8エンコーディングで3バイトを使用します。さらに、`String.length`関数は単に文字数を返すため、絵文字などのように複数のUnicodeコードポイントで構成される文字列では、意図した結果を返すとは限りません。

## 関連情報

- [String.length documentation](https://package.elm-lang.org/packages/elm-lang/core/latest/String#length)
- [Understanding String in Elm](https://elmprogramming.com/elm-string.html)