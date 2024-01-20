---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を小文字に変換するとは、文字列内の全ての大文字を対応する小文字に置換することです。これは、プログラマーが文字列マッチングや検索などを大文字と小文字を区別せずに行うために行います。

## やり方：

Elmでは、`String.toLower`関数を用いて文字列を小文字に変換します。例えば：

```Elm
import Html exposing (text)
import String 

main =
 text (String.toLower "Elm IS AWESOME!")
```

出力結果：
```
"elm is awesome!"
```

## ディープダイブ

`String.toLower`の背後にあるのはUnicodeの原則で、大文字と小文字のマッピングを管理します。伝統的に、この機能は大文字小文字を区別しない文字列の比較や検索を実現するために利用されてきました。

また、この操作の代替手段としては、個々の文字を手動で小文字に変換することが可能ですが、これは複雑さが増し、エラーを生む可能性があります。

この機能の実装内容としては、実際のところ、ElmはブラウザのJavaScriptエンジンの機能を使用しているため、詳細はブラウザの実装に依存します。

## その他の参考情報：

以下に関連するリンクを提供します。

 - [Elmの文字列操作](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
 - [文字列とUnicode](https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/)
 - [JavaScriptの toLowerCase メソッド](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)