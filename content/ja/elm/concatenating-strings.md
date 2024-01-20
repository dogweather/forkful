---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の連結とは、2つ以上の文字列を1つに結合することです。プログラマーがそれを行うのは、複数の情報片を一貫したメッセージや一つのデータ構造にまとめるためです。

## どうやる？

次のコードは、Elmで文字列を連結する基本的な例です。

```Elm
greeting : String
greeting = "おはよう、" ++ "世界！"

main : Html msg
main = Html.text greeting
```
`++` 演算子は、2つの文字列 `"おはよう、"` と `"世界！"` を連結します。結果は `"おはよう、世界！"` です。

## 深堀り

歴史的な背景から見ると、Elmの文字列連結は他の多くの関数型言語（Haskell、OCamlなど）から影響を受けています。`++` 演算子という名前は、Haskellから採用されました。

また、文字列連結の実装に関しては、Elmでは連結操作がO(1)の時間複雑度を持つ「ロープ（ropes）」というデータ構造を使用しています。これにより、大規模な文字列の操作も効率的に行うことが可能です。

また、Elmでは文字列の連結だけでなく、リストの連結も `++` 演算子を使用して行います。

```Elm
numbers : List Int
numbers = [1, 2, 3] ++ [4, 5, 6]
```

このコードはリスト `[1, 2, 3]` と `[4, 5, 6]` を連結して、新しいリスト `[1, 2, 3, 4, 5, 6]` を作ります。

## 参考資料

- [公式Elm言語ガイド - 文字列](https://guide.elm-lang.org/core_language.html#strings)
- [公式Elm言語ガイド - リスト](https://guide.elm-lang.org/core_language.html#lists)
- [公式Elm言語ガイド - ロープ（ropes）](https://guide.elm-lang.org/optimization/lazy.html)