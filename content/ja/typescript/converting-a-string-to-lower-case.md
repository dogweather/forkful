---
title:    "TypeScript: 文字列を小文字に変換する"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングで文字列を小文字に変換することの意義は、データの整合性を保つために欠かせません。例えば、入力フォームでユーザーの名前を受け取る際、大文字や小文字が混在していると同じ人物のデータと見做されず、誤った処理が行われてしまう可能性があります。そのため、文字列を小文字に統一することで、正確なデータ処理ができるようになります。

## 方法

TypeScriptで文字列を小文字に変換する方法は簡単です。以下のようなコードを使用します。

```TypeScript
let str: string = "HELLO WORLD";
let lowerCaseStr: string = str.toLowerCase();
console.log(lowerCaseStr);
```

上記のコードを実行すると、出力結果は「hello world」となります。このように、`toLowerCase()`メソッドを使用することで、文字列を小文字に変換することができます。

## 深堀り

では、実際に`toLowerCase()`メソッドはどのように文字列を小文字に変換しているのでしょうか？内部的には、文字列を文字の配列として扱い、それぞれの文字に対して`toLowerCase()`を適用して小文字に変換しています。

一般的に、大文字と小文字の間にはUnicodeで異なる数値が割り当てられています。そのため、`toLowerCase()`メソッドはこの数値を変換し、小文字の数値を返します。しかし、ごく稀に特殊な文字の変換がうまくいかないことがあります。その場合は、文字列そのものが返されることになります。

## もっと詳しく知るには

[`toLowerCase()`メソッドの仕様書（英語）](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)

[`toUpperCase()`メソッドとの違い（英語）](https://www.geeksforgeeks.org/difference-between-tolowercase-and-touppercase-function-in-javascript/)