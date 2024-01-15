---
title:                "「文字列を小文字に変換する」"
html_title:           "Haskell: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

ある文字列を小文字に変換することが必要な時があります。例えば、ユーザーが入力した文字列を大文字でも小文字でも正しく処理するために、「小文字に変換する」機能が必要になるかもしれません。
Haskellでは、文字列を変換するための多くの便利な関数が用意されていますが、今回は特に`toLower`関数を使用して文字列を小文字に変換する方法を紹介します。

## 方法

まず、プログラムの最初に`Data.Char`モジュールをインポートします。

```Haskell
import Data.Char
```

次に、`toLower`関数を使って文字列を小文字に変換します。

```Haskell
strToLower :: String -> String
strToLower = map toLower
```

上記のように、`toLower`は`map`関数を使って文字列の各文字に適用され、新しい小文字の文字列が作成されます。ここでは、`strToLower`という関数を定義しただけで、実際に文字列を小文字に変換する方法です。

例えば、次のような入力を与えたとき、

```Haskell
strToLower "HELLO"
```

次のような出力が得られます。

```Haskell
"hello"
```

## 深堀り

`toLower`関数は、Unicodeの小文字に対応しています。つまり、ラテン文字だけでなく、日本語のような文字でも正しく小文字に変換することができます。また、ASCII文字に対しても同様に動作します。

また、`toLower`関数は、文字を変換するだけでなく、単一の文字だけでなく文字列全体に適用することもできます。

例えば、次のような文字列を変換したい場合、

```Haskell
strToLower "Hello World!"
```

次のような結果が得られます。

```Haskell
"hello world!"
```

Haskellでは、関数を連結することで、複数の処理を一度に行うことができるので、`toLower`関数を組み合わせることで、さらに柔軟な文字列の変換が可能になります。

## その他参考リンク

- [HaskellのtoLower関数のドキュメント](https://hackage.haskell.org/package/base/docs/Data-Char.html#v:toLower)
- [Haskellで文字列を処理する方法](https://learnyouahaskell.com/starting-out#what-is-haskell)
- [Haskellの関数合成について](https://learnyouahaskell.com/higher-order-functions#function-composition)