---
title:                "TypeScript: 文字列の結合"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を連結することの意義についてご紹介します。文字列の連結は、プログラムをより柔軟にして簡潔に保つことができます。例えば、ユーザーの名前やメッセージを入力させるときに、個々の要素をすべて手作業で連結するのではなく、便利な方法で文字列を連結することができます。

## 連結する方法

以下に、TypeScriptで文字列を連結する方法のコード例と出力を示します。

```TypeScript
let name = "John";
let message = "Hello " + name;
console.log(message);

// Output: Hello John
```

上記のコードでは、変数`name`に"John"という文字列が代入されています。そして、`message`という変数には、文字列を連結する`+`演算子を使って、"Hello "と`name`変数を結合しています。最後に、`console.log()`メソッドを使用して、メッセージをコンソールに出力します。

また、ES6以降では、テンプレート文字列を使用しても文字列の連結ができます。これはバッククォート（`` ` ``）で囲まれた文字列の中に、`${}`を使うことで、変数や式を埋め込むことができます。

```TypeScript
let name = "John";
let message = `Hello ${name}`;
console.log(message);

// Output: Hello John
```

テンプレート文字列を使用すると、文字列中に変数や式を埋め込むことができるため、より複雑な処理を行う際に便利です。

## 深堀り

文字列を連結する方法についてもっと詳しく見ていきましょう。文字列の連結はJavaScriptやTypeScriptだけでなく、ほとんどのプログラミング言語で使用される一般的なテクニックです。そのため、覚えておくと他の言語でも役立ちます。

文字列の連結は、単に文字列を結合するのではなく、文字列を変数に代入したり、条件やループで動的に行ったりすることができるため、プログラムの柔軟性を高めることができます。

## 関連記事

[TypeScript公式ドキュメント-文字列](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)

[テンプレート文字列に関する記事](https://typescript-jp.gitbook.io/deep-dive/template-literals)

[他のプログラミング言語での文字列連結の方法](https://www.w3schools.com/js/js_strings.asp)