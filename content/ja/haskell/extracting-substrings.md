---
title:                "サブストリングの抽出"
html_title:           "Haskell: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ substring を抽出するのか

文字列処理をする際、ある部分の文字列を抜き出したい時があります。そのようなケースでは、Haskell の内部関数を使用することで簡単に文字列を抽出することができます。

## 方法

まず、文字列を定義します。例えば、"Hello, World!" という文字列を扱う場合、以下のように定義します。

```Haskell
let str = "Hello, World!"
```

次に、"World" という文字だけを抽出する方法を見ていきましょう。

```Haskell
substring 6 11 str
```

上記のコードでは、substring という内部関数を使用して、6文字目から11文字目までの部分を抽出しています。結果は "World" という文字列が返されます。

## ディープダイブ

Haskell では、文字列に対して様々な操作を行うことができます。例えば、文字列の一部を置換したり、指定した文字で分割したりすることも可能です。詳しくは公式ドキュメントを参照してください。

## 関連リンク

- [Haskell 公式ドキュメント](https://www.haskell.org/documentation/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)

## 参考文献

- [Haskell Strings](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Haskell Strings Library](http://hackage.haskell.org/package/string)