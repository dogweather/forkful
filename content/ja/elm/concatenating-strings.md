---
title:    "Elm: 文字列の連結"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
文字列を連結することに何かメリットはあるのでしょうか？実は、プログラミングではよく使われるメソッドです。ここでは、Elmで文字列を連結する方法について説明します。

## 方法
文字列を連結するには、Elmの組み込み関数である`String.concat`を使用します。以下に例を示します。

```Elm
String.concat ["Hello", " ", "World"] 
```

このコードでは、配列`["Hello", " ", "World"]`内の文字列が連結され、出力は`"Hello World"`になります。

## 深堀り
`String.concat`を使用する際には、注意点があります。例えば、空の配列を渡すとエラーが発生します。また、文字列以外のデータ型を渡すこともできません。そのため、事前に必要なデータの型を確認しておくことが重要です。

## 参考リンク

- [Elm 公式ドキュメント - String.concat](https://package.elm-lang.org/packages/elm/core/latest/String#concat)
- [初心者のためのElmチュートリアル](https://guide.elm-lang.jp/beginner/string.html)
- [Elmの文字列操作](https://blog.shinah.me/posts/elm-string-operations/)