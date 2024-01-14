---
title:                "Javascript: 「文字列を連結する」"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

「なぜ連結(concatenation)するのか」

文字列を連結することは、プログラミングにおいて非常に重要です。それでは、なぜプログラマーたちは文字列を連結するのでしょうか？それを学びましょう！

「やり方」

文字列を連結する方法は、非常にシンプルです。JavaScriptでは、"+"演算子を使用して文字列を連結できます。以下の例を見てみましょう。

```Javascript
let firstName = "太郎";
let lastName = "山田";
let fullName = firstName + lastName;

console.log(fullName);
```

出力結果は、"太郎山田"となります。

「深堀り」

JavaScriptでは、文字列を連結するために"+"演算子以外にも、"concat()"メソッドを使用することができます。また、テンプレートリテラルを使用することで、より簡潔なコードを書くことができます。

「See Also」

- JavaScriptでの文字列の連結方法: https://www.w3schools.com/js/js_strings_concat.asp
- テンプレートリテラルについて：https://www.w3schools.com/js/js_string_templates.asp