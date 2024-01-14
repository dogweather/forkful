---
title:                "TypeScript: 文字列の長さを見つける"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることには、プログラミングにおいて非常に重要な意味があります。この機能を活用することで、文字列の処理をより効率的に行うことができます。

## 方法

文字列の長さを求めるためには、lengthというプロパティを使用します。例えば、以下のようにコードを記述することで、文字列"こんにちは"の長さを求めることができます。

```TypeScript
let greeting: string = "こんにちは";
console.log(greeting.length); // 出力：5
```

## 深堀り

文字列の長さを求めるという機能は、JavaScriptとTypeScriptの両方で同じように使うことができます。しかし、JavaScriptでは文字列以外のデータ型に対してもlengthプロパティを使用することができますが、TypeScriptではエラーが発生します。これは、TypeScriptが型安全性を重視するためです。

また、文字列の長さを求める際にUnicodeに対応しているため、日本語などのマルチバイト文字列を扱うこともできます。

## 参考リンク

- [MDN Web Docs: String.length](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [TypeScript Deep Dive: String Literal Types](https://basarat.gitbooks.io/typescript/docs/types/literal-types.html)