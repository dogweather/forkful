---
title:                "Elm: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列を大文字に変換することの意義は、時にはコードの一貫性を保つため、または特定の形式に従う必要があるためです。

## 方法
```elm
import String -- 文字列操作のためのElm組み込みモジュール

capitalize : String -> String
capitalize str =
  String.toUpper str -- 「.toUpper」メソッドを使い文字列を大文字に変換

-- 使用例
capitalize "hello" -- "HELLO"
capitalize "elm" -- "ELM"
```

## 深堀り
文字列を大文字に変換することは、実際には文字列を操作するための多くの方法のうちの1つにすぎません。例えば、複数の文字列を結合することや、特定の文字列を置換することもできます。Elmの組み込みのStringモジュールには、さまざまな便利な関数が用意されているので、ぜひ調べてみてください。

## この記事が参考になる
- [Elm Language Official Website (Elm言語公式サイト)](https://elm-lang.org/)
- [Elm String Module Documentation (ElmのStringモジュールのドキュメント)](https://package.elm-lang.org/packages/elm/core/latest/String)