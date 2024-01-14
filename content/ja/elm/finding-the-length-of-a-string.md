---
title:    "Elm: 文字列の長さを求める"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# なぜ

文字列の長さを求めることに関わるモチベーションを紹介します。

## 方法

まず、文字列を定義します。

```elm
string = "Hello, World!"
```

次に、`String.length`関数を使って文字列の長さを求めます。

```elm
length = String.length string
```

最後に、求めた文字列の長さを出力します。

```elm
"Hello, World!"の長さは、" + toString length + "です。"
```

出力結果は、`Hello, World!`の長さが`13`であることを示します。

## ディープダイブ

文字列の長さを求める方法は単純ですが、Elmの内部ではどのように機能しているのでしょうか？実際には、`String.length`関数が文字列をリストとして扱い、その要素数を返しています。文字列は、1文字ずつリストの要素として格納されているため、リストの長さを求めることで文字列の長さが求められるのです。

# See Also

- [Elm Documentation](https://guide.elm-lang.jp/core_language.html#Strings)
- [Learn X in Y Minutes - Elm](https://learnxinyminutes.com/docs/ja-jp/elm/)
- [Unicode in Elm](https://elmprogramming.com/unicode-in-elm.html)