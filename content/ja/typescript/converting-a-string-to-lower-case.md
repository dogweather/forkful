---
title:    "TypeScript: 文字列を小文字に変換する"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換する必要がある理由は、データの整理や比較をする際に必要であり、より正確な結果を得るために使用されることがあります。

## 方法

```TypeScript
let str: string = "Hello World";

// 文字列を小文字に変換する
let lowerStr = str.toLowerCase();

console.log(lowerStr); // output: hello world
```

上記の例では、`toLowerCase()`メソッドを使用して文字列を小文字に変換する方法を示しています。このメソッドを使用することで、大文字と小文字を区別せずに文字列を比較することができます。

## 深堀り

文字列を小文字に変換するには、実際には英字の文字コードを変換する処理が行われます。英字の大文字は小文字よりも大きな文字コードを持っており、`toLowerCase()`メソッドはこの差を考慮して文字コードを変換することで小文字に変換します。

また、`toLowerCase()`メソッドは非ASCII文字にも対応しており、特定の言語で使用される特殊文字を小文字に変換することができます。

## See Also

- [JavaScriptで文字列を小文字に変換する方法](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [TypeScriptのデータ型](https://www.typescriptlang.org/docs/handbook/basic-types.html)
- [文字列の比較について](https://qiita.com/benit/items/6834d6ff381a2bcbd48c)