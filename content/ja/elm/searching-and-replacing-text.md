---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

文字列の検索と置換は、特定のパターンを見つけ出して新たなテキストに置き換えるプログラミングの作業の一部です。これはデータのクリーニング、フォーマットの変更、あるいはコードのメンテナンスなど、さまざまな目的で利用されます。

## どうやるの？

まず、以下のようにString.replace関数を使ってテキストの置換を行うことができます。

```Elm
module ReplaceSamples exposing(..)

import String

replaceHello : String -> String
replaceHello text =
    String.replace "Hello" "こんにちは" text

```
この関数は"Hello"を"こんにちは"に置き換えます。

## ディープダイブ

文字列の検索と置換は古くから存在し、その取り組みは正規表現の発明につながりました。正規表現を使うと、複雑なパターンの検索や置換を行うことができます。

しかし、Elmでは正規表現はサポートされていません。この問題を解決するためのいくつかの代替手法が存在します:

1.  `elm/parser` パッケージを使用してカスタムパーサーを作成する。
2.  JavaScriptとのインターロップを使用して、JavaScriptで正規表現を使用する。

## 参考にしましょう

更なる情報として、以下のリンクをご参照ください。

1. String.replaceの公式ドキュメンテーション(https://package.elm-lang.org/package/elm/core/latest/String#replace )
2. elm/parser パッケージ(https://package.elm-lang.org/packages/elm/parser/latest/ )
3. ElmとJavaScriptとのインターロップ(https://guide.elm-lang.jp/interop/ )