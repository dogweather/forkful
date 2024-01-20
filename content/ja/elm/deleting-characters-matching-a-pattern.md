---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何と、なぜ?

パターンに一致する文字を削除するとは、指定した規則に一致する全ての文字を取り除くプログラミングテクニックです。これは、データのクリーニングや加工、または特定の情報を取り除くためにプログラマーが頻繁に行います。

## 使い方：

Elmでは、文字列の特定のパターンを削除するためには "String" モジュールの "replaceAll" 関数を利用します。

```Elm
import String

removeChar : String -> String -> String
removeChar char str =
    String.replaceAll char "" str

main =
    let removeComma = removeChar ","
    in
    removeComma "こんにちは、Elm"
```

上記のプログラムの出力は:

```Elm
"こんにちはElm"
```

## より深く理解するために：

`String.replaceAll` 関数はもともとJavaScriptの `String.replace()` メソッドに由来します。JavaScriptでは正規表現の指定が可能ですが、Elmではそれよりも一貫性と安全性を重視し、正規表現を扱うことはありません。

一方、特定の文字に一致するすべての場所を置換するとは限らず、最初に見つかった一箇所だけを削除したい場合は `String.replace` 関数を使用します。この関数は最初のマッチだけを修正して、残りはそのままにします。

Elmの内部においては、文字列は内部的にJavaScriptの文字列として管理されています。そのため、文字列の操作は基本的にJavaScriptの文字列操作の効率性を引き継いでいます。

## 参考にするためのリンク：

- Elmの公式ドキュメンション：https://elm-lang.org/docs
- Elmの `String` モジュール：https://package.elm-lang.org/packages/elm/core/latest/String
- JavaScriptの `String.replace()` メソッド：https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace.