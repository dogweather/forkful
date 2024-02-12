---
title:                "文字列を小文字に変換"
aliases: - /ja/elm/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:34.726068-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-string-to-lower-case.md"
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
