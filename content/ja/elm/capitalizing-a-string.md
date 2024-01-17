---
title:                "文字列の大文字化"
html_title:           "Elm: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なに & なぜ?
文字列を大文字にすることをキャピタライズと呼び、プログラマーが行う理由は2つあります。1つ目は、ある文字列の最初の文字を大文字にすることで、より見やすく読みやすいコードを書くためです。2つ目は、プログラマーが用意したソフトウェアにユーザーが入力する文字列を大文字に変換することで、入力ミスを防ぐためです。

## 使い方:
```Elm
String.toUpper "hello" == "HELLO"
```

```Elm
String.toUpper "elm programming" == "ELM PROGRAMMING"
```

```Elm
String.toUpper "12345" == "12345"
```

## 深く掘り下げる:
キャピタライズという用語は、より古いコンピューターの時代から存在しています。当時は、大文字を書くことは紙やテープのエンコードにおいて重要な役割を果たしており、アルファベットの大文字は小文字よりも強く表現されていました。代替手段としては、String.toUpperCaseを使用することもできますが、String.toUpperの方がより一般的に使われています。実装の詳細としては、String.toUpperは指定された文字列をすべて大文字に変換することで実現されています。

## 関連リンク:
- [Elm Documentation: String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Wikipedia: Capitalization](https://ja.wikipedia.org/wiki/%E5%A4%A7%E6%96%87%E5%AD%97)