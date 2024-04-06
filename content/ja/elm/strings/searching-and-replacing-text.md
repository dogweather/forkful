---
date: 2024-01-20 17:57:51.606494-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:50:55.927940-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\
  \u63DB\u306F\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306E\u57FA\u672C\u7684\u306A\
  \u64CD\u4F5C\u3067\u3001Elm\u3067\u306F`String`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\
  \u7528\u3044\u3066\u884C\u3044\u307E\u3059\u3002\u6B74\u53F2\u7684\u306B\u3001\u3053\
  \u306E\u64CD\u4F5C\u306FUnix\u30E6\u30FC\u30C6\u30A3\u30EA\u30C6\u30A3`s/old/new/`\u69CB\
  \u6587\u3067\u77E5\u3089\u308C\u308Bsed\u30B3\u30DE\u30F3\u30C9\u306A\u3069\u3092\
  \u901A\u3058\u3066\u3082\u884C\u308F\u308C\u3066\u304D\u307E\u3057\u305F\u3002Elm\u3067\
  \u306F\u3001`String`\u306E\u95A2\u6570`replace`\u306F\u57FA\u672C\u7684\u3067\u3059\
  \u304C\u3001\u8907\u6570\u7F6E\u63DB\u306E\u305F\u3081\u306B\u306F`String.Extra.replaceAll`\u3092\
  \u5229\u7528\u3059\u308B\u3068\u4FBF\u5229\u3067\u3059\u3002\u305F\u3060\u3057\u3001\
  `String.Extra`\u306F\u30C7\u30D5\u30A9\u30EB\u30C8\u3067\u306FElm\u306B\u542B\u307E\
  \u308C\u3066\u3044\u306A\u3044\u305F\u3081\u3001\u8FFD\u52A0\u306E\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u304C\u5FC5\u8981\u3068\u306A\u308A\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

## How to: (やり方)
```Elm
module Main exposing (main)

import Browser
import Html exposing (Html, text)
import String

replaceText : String -> String -> String -> String
replaceText needle replacement haystack =
    String.Extra.replaceAll needle replacement haystack

main : Html msg
main =
    text (replaceText "world" "Elm" "Hello, world!")
```
コンソール出力:
```
Hello, Elm!
```

## Deep Dive (掘り下げ)
テキストの検索と置換はプログラミングの基本的な操作で、Elmでは`String`モジュールを用いて行います。歴史的に、この操作はUnixユーティリティ`s/old/new/`構文で知られるsedコマンドなどを通じても行われてきました。Elmでは、`String`の関数`replace`は基本的ですが、複数置換のためには`String.Extra.replaceAll`を利用すると便利です。ただし、`String.Extra`はデフォルトではElmに含まれていないため、追加のライブラリが必要となります。

簡単な例を示しましたが、実際には複雑なパターンマッチングや正規表現が必要な場合もあります。Elmでは、完全な正規表現のサポートは意図的に限定されており、より関数型的なアプローチに重きを置いています。

## See Also (関連情報)
- [Elm String Module Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [elm-community/string-extra](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)
- `sed`の詳細: [GNU sed manual](https://www.gnu.org/software/sed/manual/sed.html)
