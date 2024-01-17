---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何でしょう？何故ですか？

プログラムの中で、文字列の長さを調べるというのは、機能であり、プログラマーが文字列の操作や処理を行う上で重要なスキルです。

## 方法：

Elmのプログラムで文字列の長さを調べる方法は、文字列をリストとして扱い、そのリストの長さを調べることで可能です。

```
getStringLength: String -> Int
getStringLength str =
  List.length (String.toList str)

main =
  Elm.io.print (getStringLength "Hello") --> 5
```

## 詳しく調べる

文字列の長さを調べる機能は、プログラミング言語の中でも非常に一般的であり、多くの言語で同様の方法で実装されています。また、リストを用いる方法以外にも、文字列の長さを調べるためのアルゴリズムは様々あります。文字列の長さを調べることで、文字列の中で特定の文字や単語の出現位置を探すなど、より複雑なプログラムの実装にも役立ちます。

## 関連リンク

- [Elm Documentation: Strings](https://elm-lang.org/docs/strings)
- [Rosetta Code: String Length](https://rosettacode.org/wiki/String_length)