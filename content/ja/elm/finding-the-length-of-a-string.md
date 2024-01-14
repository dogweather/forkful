---
title:                "Elm: 文字列の長さを求める方法"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ？

文字列の長さを求めることの意義を説明するために、エルムプログラミングのブログ投稿を書きます。

## 方法

```Elm
let str = "こんにちは"
in
String.length str
-- 5
```

文字列の長さを求めるには、`String.length`関数を使います。この関数は、渡された文字列の長さを返します。例として、上記のコードでは、文字列"こんにちは"の長さは5であることがわかります。

## 深堀り

文字列の長さを求める方法についてさらに詳しく説明します。文字列の長さは、文字の個数を数えることで求めることができます。エルムでは、文字列は1文字ごとにコードポイントという数字で表されます。そのため、文字列の長さはコードポイントの数に等しくなります。

## 参考リンク

- [ElmのStringモジュールドキュメント](https://package.elm-lang.org/packages/elm/core/latest/String)
- [エルムの文字列操作について学ぶ](https://dev.classmethod.jp/articles/learn-elm-with-string-manipulations/)