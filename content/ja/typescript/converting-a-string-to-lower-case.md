---
title:                "TypeScript: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換する作業は、プログラム内で文字列を比較や検索する際に非常に便利です。大文字と小文字を区別しない場合、文字列を小文字に変換することで簡単に処理できるようになります。

## 方法

文字列を小文字に変換するためには、JavaScript内で用意されている `toLowerCase()` メソッドを使用します。例えば、以下のように記述することで文字列を小文字に変換することができます。

```TypeScript
let str = "Hello World";
let lowerCaseStr = str.toLowerCase();

console.log(lowerCaseStr);
// 出力結果：hello world
```

## 深堀り

JavaScriptの `toLowerCase()` メソッドは、文字列を小文字に変換するだけではなく、様々な文字列操作を行うことができます。例えば、英数字以外の文字も自動的に小文字に変換するため、多言語を取り扱うプログラムでも便利に使うことができます。また、`toUpperCase()` メソッドを併用することで、文字列内の大文字のみを小文字に変換することも可能です。さらに、`toLocaleLowerCase()` メソッドを使用することで、ロケールに応じた正しい小文字変換を行うこともできます。

## 参考リンク

[JavaScriptで文字列を操作する方法 | MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String)

「See Also」
- [JavaScriptで文字列を比較する方法 | MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Operators/String)
- [TypeScriptで配列を操作する方法 | チュートリアルジャパン](https://www.tutorialspoint.com/typescript/typescript_strings.htm)