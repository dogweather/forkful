---
title:                "TypeScript: 文字列を小文字に変換する"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することに関して、なぜそれをする必要があるのかということが気になるかもしれません。実際、私たちはしばしば文字列を小文字で使用したい場合があります。例えば、ユーザーからの入力を検証する際や、特定の検索キーワードを探す際に、文字列を小文字に変換する必要があるかもしれません。そのため、効率的かつ正確なコードを書くために、文字列を小文字に変換する方法を知ることは重要です。

## 方法

TypeScriptでは、文字列を小文字に変換するためには`toLowerCase()`メソッドを使用します。このメソッドは文字列オブジェクトに内蔵されており、元の文字列を小文字に変換した新しい文字列を返します。以下のコードブロックを参考にしてください。

```TypeScript
let str: string = "HELLO WORLD";
let lowerCaseStr: string = str.toLowerCase();
console.log(lowerCaseStr); // 出力結果: hello world
```

## 深堀り

TypeScriptにおける`toLowerCase()`メソッドは、文字列を小文字にするだけではなく、大文字や小文字を区別する言語に特化した変換を行うこともできます。また、特定の言語環境に依存した変換も行うことができます。詳細についてはドキュメントを参照してください。

## 併せて参照

見るべき他の関連記事やリンクを以下に挙げます。 
- [TypeScriptドキュメント](https://www.typescriptlang.org/docs/handbook/strings.html)
- [MDN Web Docs - `toLowerCase()`](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)