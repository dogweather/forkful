---
title:                "Elm: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
プログラマーの中には、文字列を小文字に変換する必要がある場合があります。例えば、データの検索や比較を行う際に、大文字と小文字を区別したくない場合などです。Elmでは、簡単に文字列を小文字に変換することができるので、この技術を学ぶことは重要です。

## 方法
文字列を小文字に変換するには、StringモジュールのtoLower関数を使います。例えば、次のように使用します。

```Elm
import String

sampleString = "ELM PROGRAMMING"

lowercaseString = String.toLower sampleString

```

このコードを実行すると、"ELM PROGRAMMING"が"elm programming"に変換されます。

## 深堀り
Elmでは、文字列を操作するための便利な関数が多数用意されています。しかし、StringモジュールのtoLower関数は単純で、特に深い解説を必要としません。ただ、文字列を小文字に変換することに加えて、toLower関数には文字列内のUniode文字をASCIIに変換する機能もあります。

## 詳しくは
詳しい情報やElmの文字列を操作するための他の便利な関数については、次のリンクを参考にしてください。

[Elm Strings - Elm Guide](https://guide.elm-lang.org/strings/)

[Elm Stringモジュール - Elmドキュメント](https://package.elm-lang.org/packages/elm-lang/core/latest/String)

[The Power of Elm Strings - Charlie Koster](https://dev.to/charliekoster/gotchas-in-the-elm-string-modules-a-hitchhiker-s-guide-52df)

[Elm String Operations - TomaEasy](https://github.com/TomaEasy/elm-string-operations)

## 他のリソースを参照する
[Learn Elm - 公式Elmドキュメント](https://elm-lang.org/docs)

[String toLowerCase - Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)