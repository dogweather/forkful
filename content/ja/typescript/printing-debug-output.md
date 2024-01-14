---
title:    "TypeScript: 「デバッグ出力の印刷」"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

デバッグのためにデバッグ出力をプリントする必要があるか簡単に説明します。

デバッグ出力をプリントすると、コードが実行されるときに何が起こっているのかをより詳細に知ることができます。これにより、問題箇所を特定し、修正するのに役立ちます。

## やり方

```TypeScript
console.log("こんにちは、世界！");

// 出力: こんにちは、世界！
```

デバッグ出力をプリントするには、`console.log()`メソッドを使用します。これは、コンソールに任意の値を出力するために使用されます。上記の例では、文字列「こんにちは、世界！」がコンソールにプリントされます。

また、`console.log()`メソッドを使用して、変数の値や関数の戻り値を確認することもできます。

```TypeScript
let num = 10;
console.log(num); 

// 出力: 10

function add(a: number, b: number) {
  return a + b;
}

console.log(add(5, 3));

// 出力: 8
```

## ディープダイブ

コンソールに出力される内容は、ブラウザの開発者ツールのコンソールタブに表示されます。ここには、コードを実行する際に生成されたエラーや警告も表示されます。また、`console.log()`メソッド以外にも、`console.error()`や`console.warn()`などのメソッドを使用して、より重要な情報を表示することもできます。

厳密なデータ型を使用している場合、デバッグ出力において重要な情報を失わないために、`console.log()`の代わりに`console.info()`メソッドを使用することもできます。

さらに、`console.group()`と`console.groupEnd()`メソッドを使用して、出力をグループ化することができます。この機能は、複雑なコードのデバッグ時に特に役立ちます。

## 他に見るもの

- [TypeScript 公式ドキュメント](https://www.typescriptlang.org/docs/)
- [TypeScript チュートリアル (日本語訳)](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- [TypeScript ブログ](https://devblogs.microsoft.com/typescript/)