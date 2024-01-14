---
title:                "Elm: 部分文字列の抽出"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why
なぜサブストリングを抽出した方が良いのか？

サブストリングを抽出することが、Web開発やデータ処理を行う際に非常に有用です。特定のテキストや文字列から、必要な情報だけを抽出することができます。これにより、データの収集や整理がより効率的に行えます。

## How To
サブストリングを抽出する方法をご紹介します。「```Elm ... ```」のコードブロックを使用し、実際のコーディング例と出力を確認していきましょう。

### 例題1: 文字列の一部を抽出する
```
Elm String.slice "Hello World" 6 11
```
```
"Just"
```

この例では、String.sliceメソッドを使用して、"Hello World"という文字列から、インデックス6から11までの部分文字列を抽出しています。これにより、最初の単語"Hello"を除いた「World」という部分文字列が取得できます。

### 例題2: 正規表現を使用して特定のパターンを抽出する
```
Elm Regex.find Regex.All "Hello 123 World" "([0-9]+)"
```
```
Just [ "123" ]
```

正規表現を使用することで、特定のパターンを持つ文字列を抽出することができます。上記の例では、"Hello 123 World"という文字列から数字の部分文字列を抽出しています。

## Deep Dive
サブストリングを抽出する方法についてもっと詳しく知りたい方は、[Elmの公式ドキュメント](https://elm-lang.org/docs)をご覧ください。また、[elm/regexパッケージ](https://package.elm-lang.org/packages/elm/regex/latest/)を使用することで、より高度な文字列抽出を行うことができます。

## See Also
参考情報:

- [Elmの公式ドキュメント](https://elm-lang.org/docs)
- [elm/regexパッケージ](https://package.elm-lang.org/packages/elm/regex/latest/)

サブストリングを抽出することで、より柔軟なデータ処理が可能になります。ぜひ、上記の方法を参考にしてみてください。