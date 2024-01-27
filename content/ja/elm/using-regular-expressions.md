---
title:                "正規表現の使用"
date:                  2024-01-19
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ?)
正規表現は、文字列内パターン検索と操作のための構文です。プログラマは、テキストデータのバリデーションや解析、変更を効率的にするために使います。

## How to: (方法)
Elmにおける正規表現の使用例です：
```Elm
import Regex exposing (Regex, fromString, find, FindResult)

-- 正規表現を作成
regex : Regex
regex = Regex.fromString "\\d+" |> Maybe.withDefault (Regex.fromString "" |> Maybe.withDefault Regex.never)

-- 文字列から数字を検索
findNumbers : String -> List String
findNumbers str =
    find regex str |> List.map .match

-- 結果のサンプル
sampleOutput : List String
sampleOutput = findNumbers "今日の温度は23度です"

-- sampleOutput の期待される出力：["23"]
```

## Deep Dive (深い潜入)
正規表現は1960年代に起源を持ち、Elmでは`Regex`モジュールを通じて提供されています。`String.contains`, `String.startsWith`, または`String.endsWith`関数を使うことで単純なパターンマッチングの代替が可能ですが、複雑な検索には不向きです。Elmの正規表現はJavaScriptの正規表現構文を基にしていて、`find`や`replace`などの関数を使って操作します。

## See Also (関連項目)
- Elmの`Regex`モジュールドキュメント: https://package.elm-lang.org/packages/elm/regex/latest/Regex
- JavaScriptの正規表現ガイド (Elmの正規表現はJavaScriptに基づいています。): https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions
- Elmコミュニティのディスカッションとサポート: https://discourse.elm-lang.org/
