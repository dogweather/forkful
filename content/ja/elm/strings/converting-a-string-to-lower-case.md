---
date: 2024-01-20 17:38:34.726068-07:00
description: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u306E\u306F\u3001\u5168\u3066\u306E\u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\u3059\
  \u308B\u51E6\u7406\u3067\u3059\u3002\u6587\u5B57\u306E\u5927\u6587\u5B57\u5C0F\u6587\
  \u5B57\u3092\u63C3\u3048\u305F\u308A\u3001\u5927\u6587\u5B57\u304C\u6DF7\u5728\u3057\
  \u3066\u3044\u308B\u306E\u3092\u7D71\u4E00\u3057\u305F\u308A\u3059\u308B\u305F\u3081\
  \u306B\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.989232-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u306E\u306F\u3001\u5168\u3066\u306E\u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\u3059\
  \u308B\u51E6\u7406\u3067\u3059\u3002\u6587\u5B57\u306E\u5927\u6587\u5B57\u5C0F\u6587\
  \u5B57\u3092\u63C3\u3048\u305F\u308A\u3001\u5927\u6587\u5B57\u304C\u6DF7\u5728\u3057\
  \u3066\u3044\u308B\u306E\u3092\u7D71\u4E00\u3057\u305F\u308A\u3059\u308B\u305F\u3081\
  \u306B\u4F7F\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
文字列を小文字に変換するのは、全ての文字を小文字にする処理です。文字の大文字小文字を揃えたり、大文字が混在しているのを統一したりするために使います。

## How to: (方法)
Elmでは、`String` モジュールの `toLower` 関数を使って文字列を小文字に変換できます。

```Elm
import String

lowerCaseString : String -> String
lowerCaseString str =
    String.toLower str

-- 使用例
lowerCaseExample : String
lowerCaseExample =
    lowerCaseString "Hello, World!"

-- 出力: "hello, world!"
```

## Deep Dive (深い潜水)
文字列を小文字にするのは、プログラミングが生まれた初期からある基本的な操作です。Elmや他の多くのプログラミング言語では、文字列操作関数を提供しています。

それぞれの言語やライブラリによって、この機能は異なる方法で実装されることがありますが、ElmではUnicodeを適切に扱う`String.toLower`関数が提供されており、世界中のさまざまな文字に対応しています。

代替手段としては、手動で変換するなどが考えられますが、エラーの可能性が高く、非効率です。そのため、組み込み関数を使うのが一般的です。

## See Also (関連情報)
- Elm公式ドキュメントの`String`モジュール: https://package.elm-lang.org/packages/elm/core/latest/String#toLower
- Unicodeについての詳細情報: http://unicode.org/
- 文字列処理の基礎について学ぶ:https://www.elm-tutorial.org/en/02-elm-arch/06-strings.html
