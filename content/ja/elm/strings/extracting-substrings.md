---
date: 2024-01-20 17:45:27.894137-07:00
description: "How to: (\u3084\u308A\u65B9:) Elm\u3067\u90E8\u5206\u6587\u5B57\u5217\
  \u3092\u53D6\u5F97\u3059\u308B\u306B\u306F\u3001`String.slice` \u95A2\u6570\u3092\
  \u4F7F\u3044\u307E\u3059\u3002`slice start end str` \u306F\u3001`str` \u3067\u6307\
  \u5B9A\u3055\u308C\u305F\u6587\u5B57\u5217\u304B\u3089 `start` \u304B\u3089 `end`\
  \ \u306E\u7BC4\u56F2\u306E\u90E8\u5206\u6587\u5B57\u5217\u3092\u8FD4\u3057\u307E\
  \u3059\uFF08`start` \u306F\u542B\u307E\u308C\u3001`end`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.880133-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9:) Elm\u3067\u90E8\u5206\u6587\u5B57\u5217\u3092\u53D6\
  \u5F97\u3059\u308B\u306B\u306F\u3001`String.slice` \u95A2\u6570\u3092\u4F7F\u3044\
  \u307E\u3059\u3002`slice start end str` \u306F\u3001`str` \u3067\u6307\u5B9A\u3055\
  \u308C\u305F\u6587\u5B57\u5217\u304B\u3089 `start` \u304B\u3089 `end` \u306E\u7BC4\
  \u56F2\u306E\u90E8\u5206\u6587\u5B57\u5217\u3092\u8FD4\u3057\u307E\u3059\uFF08`start`\
  \ \u306F\u542B\u307E\u308C\u3001`end` \u306F\u542B\u307E\u308C\u307E\u305B\u3093\
  \uFF09\u3002`end` \u304C\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u8D85\u3048\u308B\
  \u5834\u5408\u3001\u7D50\u679C\u306E\u6587\u5B57\u5217\u306F `start` \u304B\u3089\
  \u6587\u5B57\u5217\u306E\u7D42\u308F\u308A\u307E\u3067\u306B\u306A\u308A\u307E\u3059\
  \u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

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
