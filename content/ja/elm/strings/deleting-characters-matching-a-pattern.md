---
date: 2024-01-20 17:41:56.965612-07:00
description: "How to: (\u65B9\u6CD5) Elm\u3067\u306F\u3001\u6B63\u898F\u8868\u73FE\
  \u306F\u76F4\u63A5\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u305B\u3093\u304C\
  \u3001`String`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u3063\u3066\u30D1\u30BF\
  \u30FC\u30F3\u306B\u30DE\u30C3\u30C1\u3059\u308B\u6587\u5B57\u3092\u524A\u9664\u3059\
  \u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u4F8B\u3092\
  \u793A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.874421-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Elm\u3067\u306F\u3001\u6B63\u898F\u8868\u73FE\u306F\u76F4\
  \u63A5\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u305B\u3093\u304C\u3001`String`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u3063\u3066\u30D1\u30BF\u30FC\u30F3\u306B\u30DE\
  \u30C3\u30C1\u3059\u308B\u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u4F8B\u3092\u793A\u3057\u307E\u3059\
  \u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

## How to: (方法)
Elmでは、正規表現は直接サポートしていませんが、`String`モジュールを使ってパターンにマッチする文字を削除することができます。以下に例を示します。

```Elm
module Main exposing (..)
import Html exposing (text)
import String

removePattern : String -> String -> String
removePattern pattern string =
    String.split pattern string |> String.join ""

main =
    let
        originalString = "Hello 123 Elm!"
        cleanedString = removePattern "123 " originalString
    in
    text cleanedString  -- "Hello Elm!"
```

実行結果は、"Hello Elm!" が出力されます。

## Deep Dive (深堀り)
Elmは関数型プログラミング言語であり、JavaScriptのように正規表現を使って直接パターンマッチングを行うことはできません。しかし、`String`モジュールの関数を組み合わせれば、同様の結果を達成できます。`removePattern`関数は、`String.split`を使って文字列をパターンで分割し、`String.join`で再結合します。これがパターンにマッチする文字を削除するElm特有の方法です。代わりに`elm/regex`パッケージを使うこともできますが、それはより複雑なパターンマッチングに必要です。

## See Also (関連情報)
- Elmの`String`モジュールのドキュメント: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
- elm/regex パッケージ: [https://package.elm-lang.org/packages/elm/regex/latest](https://package.elm-lang.org/packages/elm/regex/latest)
