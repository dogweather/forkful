---
title:                "パターンにマッチする文字を削除する"
html_title:           "Elm: パターンにマッチする文字を削除する"
simple_title:         "パターンにマッチする文字を削除する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

あなたは、特定のパターンにマッチする文字を削除することが必要になるかもしれません。そのような場合、ループや条件分岐を使わずに簡単に実現できる方法をご紹介します。

## 方法

まず、`List.filter`関数を使用して削除したい文字を除外します。次に、`String.join`関数を使ってリスト内の文字を結合し、最終的な文字列を作成します。

```Elm
deleteCharacters : String -> String
deleteCharacters str =
    str
        |> String.toList
        |> List.filter (\c -> c /= 'a') -- 文字をフィルタリング
        |> String.fromList
```

上記の例では、文字列中の`a`を削除することができます。他の文字を削除したい場合は、`c`の条件を変更すれば良いでしょう。

例えば、`List.filter (\c -> c /= ' ' && c /= '.')`とすれば、空白とピリオドを削除することができます。

`str`に`"Hello, world!"`と入力した場合、出力は`"Helloworld"`になるはずです。

## 深堀り

この方法は、文字列をリストに変換してから処理することで実現されます。文字列をリストに変換するコストはほとんどありませんが、文字列処理の場合は少し気軽に使うようにしてください。

## 参考

- [`String.filter` - Elm パッケージドキュメント](https://package.elm-lang.org/packages/elm/core/latest/String#filter)
- [`List.filter` - Elm パッケージドキュメント](https://package.elm-lang.org/packages/elm/core/latest/List#filter)
- [`String.toList` - Elm パッケージドキュメント](https://package.elm-lang.org/packages/elm/core/latest/String#toList)
- [`String.fromList` - Elm パッケージドキュメント](https://package.elm-lang.org/packages/elm/core/latest/String#fromList)