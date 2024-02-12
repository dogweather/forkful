---
title:                "文字列を小文字に変換"
aliases:
- /ja/javascript/converting-a-string-to-lower-case/
date:                  2024-01-20T17:38:47.081462-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
文字列を小文字に変換するとは、全てのアルファベット文字を小文字にすることです。この操作は、大文字と小文字を区別しない比較を行ったり、統一的なデータ形式を保ったりするために行われます。

## How to: (方法)
JavaScriptで文字列を小文字に変換するには、`toLowerCase()` メソッドを使います。以下はコード例と出力サンプルです。

```javascript
let greeting = "こんにちは、WORLD!";
let lowerCaseGreeting = greeting.toLowerCase();

console.log(lowerCaseGreeting); // "こんにちは、world!"
```

## Deep Dive (掘り下げ)
JavaScriptが最初に登場した頃から、文字列操作は基本的な機能の1つです。`toLowerCase()` メソッドは、ECMAScript標準の初期版において定義されており、今日に至るまで広く使われています。

代替手段として、正規表現と `replace()` メソッドを組み合わせることもできますが、これは非効率であり、単純な小文字変換には `toLowerCase()` の方が適しています。

小文字変換の実装はブラウザやJavaScriptエンジンによって異なりますが、Unicode標準に従い、さまざまな言語や特殊文字に対応していることが期待されます。

## See Also (関連情報)
- MDN Web Docsの `toLowerCase()` の解説: https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- ECMAScript標準の最新版: https://www.ecma-international.org/ecma-262/
