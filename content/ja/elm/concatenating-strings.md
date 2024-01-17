---
title:                "「文字列の連結」"
html_title:           "Elm: 「文字列の連結」"
simple_title:         "「文字列の連結」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

「## 何をして，なぜ？」

文字列を連結することは，文字列を合体させて，新しい文字列を作ることです．これは，新しいコンテンツを作ったり，情報をまとめたりするために，プログラマーが行います．

「## 方法」

Elmでは，```String.concat```を使って，文字列を連結することができます．例えば，

```Elm
String.concat ["Hello, ", "world!"]
```

とすることで，"Hello, world!"という文字列が作られます．この関数では，引数として文字列をリストとして渡します．また，```String.join```を使って，区切り文字を指定し，文字列を繋げることもできます．例を見てみましょう．

```Elm
String.join " " ["Hello,", "world!"]
```

出力は，"Hello, world!"となります．

「## 詳しく見る」

文字列を連結する方法は，プログラミングにおいて非常に一般的なものです．Elmの他にも，JavaScriptやPythonなどの言語でも同様の方法で文字列を連結することができます．ただし，パフォーマンスによる違いがあるので，注意が必要です．

また，文字列を連結する際には，注意が必要な点もあります．例えば，大量の文字列を連結する場合，パフォーマンスの低下が起こる可能性があります．このような場合は，代わりに```String.join```を使用することで，パフォーマンスを向上させることができます．

「## 参考」

- Elm Documentation - ```String.concat```: https://package.elm-lang.org/packages/elm/core/latest/String#concat
- Elm Documentation - ```String.join```: https://package.elm-lang.org/packages/elm/core/latest/String#join