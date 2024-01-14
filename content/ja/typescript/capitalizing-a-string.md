---
title:                "TypeScript: 文字列の大文字化"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の大文字化に取り組む理由を1〜2文で説明します。

文字列の大文字化は、プログラミングで非常によく使用されるタスクです。例えば、ユーザーから入力された名前やメッセージを、大文字で表示したい場合があります。また、ソートや比較の際には大文字小文字を区別する必要があるため、文字列の大文字化は必要不可欠です。

## 使い方

文字列の大文字化をするには、簡単なコードを書くだけで実現できます。まずは、文字列を受け取って大文字化する関数を定義しましょう。

```TypeScript
function capitalizeString(str: string): string {
  return str.toUpperCase();
}

console.log(capitalizeString("hello")); // HELLO
```

上記のコードでは、`capitalizeString`という関数を定義し、`toUpperCase`メソッドを使って受け取った文字列を大文字化しています。そして、`console.log`で結果を表示しています。

また、TypeScriptでは文字列に対して`[ ]`を使うことで、指定した位置の文字を取り出すことができます。そのため、任意の位置の文字を大文字にしたい場合は以下のようにコードを書くことができます。

```TypeScript
function capitalizeString(str: string, position: number): string {
  let char: string = str[position];
  return str.replace(str[position], char.toUpperCase());
}

console.log(capitalizeString("hello", 0)); // Hello
```

上記のコードでは、`capitalizeString`関数に加えて、`position`という引数を指定しています。そして、`replace`メソッドを使って指定した位置の文字を大文字に変更しています。こうすることで、任意の位置の文字を大文字に変換することができます。

## 詳細を見る

文字列の大文字化について、さらに詳しく説明します。

文字列は文字の集まりであり、それぞれの文字は特定の位置に存在しています。そのため、文字列を取り扱う際には位置に注目することが重要です。文字列の大文字化も、同じように位置を意識してプログラムを書くことで実現できます。

また、TypeScriptでは文字列型の変数に対して`toUpperCase`メソッドだけでなく、`charAt`や`replace`など様々なメソッドが使えるため、より柔軟に文字列の操作ができます。

## 属している

## もっと詳しく知りたい方のために、以下のリンクを参考にしてください。

- [TypeScript Documentaton](https://www.typescriptlang.org/docs/)
- [文字列の扱い方 | MDN(日本語)](https://developer.mozilla.org/ja/docs/Web/JavaScript/String)
- [TypeScript で文字列を処理する方法について | Qiita(日本語)](https://qiita.com/_takeshi_24/items/cd4027df5ca3012f0caf)
- [TypeScript: Trim a string and capitalize the first letter | Stack Overflow(en)](https://stackoverflow.com/questions/29504398/typescript-trim-a-string-and-capitalize-the-first-letter)