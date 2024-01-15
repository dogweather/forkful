---
title:                "文字列を連結する"
html_title:           "Elm: 文字列を連結する"
simple_title:         "文字列を連結する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

あなたが今この記事を読んでいるのは、おそらくElmプログラミング言語を学ぶためです。それでは、なぜ私たちは文字列を連結する必要があるのでしょうか？文字列は、テキストデータを処理するための重要な要素です。たとえば、ウェブ開発では、ユーザーからの入力データを文字列として受け取り、それを処理してHTML要素に表示する必要があります。文字列を連結することで、複数の文字列を1つの文字列にまとめることができ、より複雑な処理が可能になります。

## How To

文字列の連結は、Elmコードの中でもよく使われる操作です。簡単な例を見てみましょう。

```Elm
name = "John"
greeting = "Hello " ++ name
```

このコードでは、`name`という変数に"John"という文字列が、`greeting`という変数に"Hello John"という文字列が格納されます。`++`演算子を使うことで、2つの文字列を結合することができます。

さらに複雑な例を見てみましょう。

```Elm
first_name = "John"
last_name = "Doe"
full_name = first_name ++ " " ++ last_name
```

この例では、2つの変数を使って1つの文字列を作成しています。最後の行では、`++`演算子を使って3つの文字列を結合し、`full_name`という変数に"John Doe"という文字列を格納しています。

## Deep Dive

Elmでは、文字列を結合する様々な方法があります。まず、`++`演算子以外にも、`String.concat`関数を使って文字列を連結することができます。また、`String.join`関数を使うことで、引数に与えられた区切り文字を使って複数の文字列を結合することができます。

さらに、`String.concatMap`関数を使うことで、リスト内の全ての文字列を連結することもできます。この関数では、関数を引数として与える必要があります。

## See Also

- [Elm Documentation: String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Mastering String Manipulation in Elm](https://www.erikschierboom.com/2018/10/31/mastering-string-manipulation-in-elm/)
- [Learn Elm in Y Minutes](https://learnxinyminutes.com/docs/elm/)