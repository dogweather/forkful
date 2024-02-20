---
date: 2024-01-20 17:57:51.606494-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\
  \u7279\u5B9A\u306E\u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u3066\u4ED6\u306E\u6587\
  \u5B57\u5217\u3067\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u306E\u4FEE\u6B63\u3001\
  \u30C7\u30FC\u30BF\u306E\u6574\u5F62\u3001\u307E\u305F\u306F\u7279\u5B9A\u306E\u30D1\
  \u30BF\u30FC\u30F3\u306E\u66F4\u65B0\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.144814
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\
  \u7279\u5B9A\u306E\u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u3066\u4ED6\u306E\u6587\
  \u5B57\u5217\u3067\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u306E\u4FEE\u6B63\u3001\
  \u30C7\u30FC\u30BF\u306E\u6574\u5F62\u3001\u307E\u305F\u306F\u7279\u5B9A\u306E\u30D1\
  \u30BF\u30FC\u30F3\u306E\u66F4\u65B0\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストの検索と置換は、特定の文字列を見つけて他の文字列で置き換えることです。プログラマーは、コードの修正、データの整形、または特定のパターンの更新のためにこれを行います。

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
