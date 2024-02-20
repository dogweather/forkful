---
date: 2024-01-20 17:45:27.894137-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u90E8\u5206\u6587\u5B57\u5217\u3092\u62BD\
  \u51FA\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u306F\u30C7\u30FC\u30BF\u51E6\u7406\u3084\u30C6\u30AD\u30B9\u30C8\u5206\
  \u6790\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.149990
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u90E8\u5206\u6587\u5B57\u5217\u3092\u62BD\
  \u51FA\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u306F\u30C7\u30FC\u30BF\u51E6\u7406\u3084\u30C6\u30AD\u30B9\u30C8\u5206\
  \u6790\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
---

{{< edit_this_page >}}

## What & Why? (何とその理由?)
文字列から部分文字列を抽出することを指します。プログラマはデータ処理やテキスト分析のためにこれを行います。

## How to: (やり方:)
Elmで部分文字列を取得するには、`String.slice` 関数を使います。`slice start end str` は、`str` で指定された文字列から `start` から `end` の範囲の部分文字列を返します（`start` は含まれ、`end` は含まれません）。`end` が文字列の長さを超える場合、結果の文字列は `start` から文字列の終わりまでになります。

```Elm
import String

substringExample : String
substringExample =
  String.slice 0 5 "Hello, World!"

-- "Hello"

substringExample2 : String
substringExample2 =
  String.slice 7 12 "Hello, World!"

-- "World"
```

## Deep Dive (詳細な解析)
Elmでは文字列操作が言語の設計からシンプルにされています。`String.slice`関数は内部的にJavaScriptの`slice()`メソッドを利用して高速に動作します。この関数に関する重要な歴史的背景は、Elmが常にパフォーマンスと安全性に焦点を当てていることです。例えば、インデックスが不正な場合は空の文字列を返します。`String.sub`や`String.left`といった代替関数もありますが、`String.slice`が最も一般的に使われる方法です。

## See Also (関連情報)
- Elmの公式`String` モジュールのドキュメント: [http://package.elm-lang.org/packages/elm-lang/core/latest/String](http://package.elm-lang.org/packages/elm-lang/core/latest/String)
- JavaScriptの`slice()` メソッドについての詳細な解説: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
