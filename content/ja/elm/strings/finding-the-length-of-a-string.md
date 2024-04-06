---
date: 2024-01-20 17:47:18.904274-07:00
description: "How to: Elm\u3067\u306F`String.length`\u95A2\u6570\u3092\u4F7F\u3063\
  \u3066\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u307E\u3059\u3002\
  \u3068\u3066\u3082\u30B7\u30F3\u30D7\u30EB\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.882852-06:00'
model: gpt-4-1106-preview
summary: "Elm\u3067\u306F`String.length`\u95A2\u6570\u3092\u4F7F\u3063\u3066\u6587\
  \u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u307E\u3059\u3002\u3068\u3066\
  \u3082\u30B7\u30F3\u30D7\u30EB\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

## How to:
Elmでは`String.length`関数を使って文字列の長さを見つけます。とてもシンプルです。

```Elm
import Html exposing (text)

main =
  let
    myString = "こんにちは"
  in
    text (String.fromInt (String.length myString))

-- 出力: "5"
```

## Deep Dive
文字列の長さを見つける方法は、Elmの初期バージョンから存在しています。Unicode対応のため、ElmではUTF-16を使って文字列をエンコードしています。これ意味することは？サロゲートペア（絵文字など特別な文字）が2つのコードユニットとして数えられる場合があるということです。代替手段？JavaScriptの`String.length`を直接使う手もありますがElmの関数は型安全性と純粋関数の恩恵を受けます。実装の詳細？Elmは内部で文字列を効果的に処理するため、ほとんどのケースではパフォーマンスに問題はありません。

## See Also
- Elmの`String`モジュールの公式ドキュメント：[Elm String Module](http://package.elm-lang.org/packages/elm/core/latest/String)
- UnicodeとJavaScriptのサロゲートペアについての詳細：[Unicode and JavaScript](https://mathiasbynens.be/notes/javascript-unicode)
