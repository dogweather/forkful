---
title:    "Elm: テキストの検索と置換"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## なぜ

テキストの検索と置き換えをする理由は様々です。コードの繰り返しを減らし、作業を効率化するために使用することができます。

## 方法

```elm
import String

main =
    let
        str = "Hello, world!"
        replacedStr = String.replace "world" "Elm" str
    in
        text replacedStr
```

この例では、"Hello, world!"という文字列から"world"を"Elm"に置き換えています。`String`モジュールの`replace`関数を使用し、置き換えた結果を`text`で表示します。

## 深堀り

Elmでは、`String`モジュールの他にも`Regex`を使うことでより複雑な文字列の検索と置き換えが可能です。また、`elm-format`を使用することでコードのフォーマットを自動で行うことができます。

## 関連情報

- [Stringモジュールのドキュメント](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Regexモジュールのドキュメント](https://package.elm-lang.org/packages/elm/regex/latest/)
- [elm-formatの使い方](https://github.com/avh4/elm-format#usage)