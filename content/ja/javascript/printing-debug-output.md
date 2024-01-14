---
title:    "Javascript: 「デバッグ出力のプリント」"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## なぜ

デバッグ出力をプリントするメリットは一言で言えば、コードをデバッグすることがより簡単になることです。デバッグ出力をプリントすることによって、どのような値が変数に格納されているのかを確認することができます。

## 方法

デバッグ出力をプリントするには、デバッガーのような特別なツールを使用する必要はありません。代わりに、`console.log()`メソッドを使用してコンソールにメッセージをプリントすることができます。

例えば、以下のコードを考えてみましょう。

```Javascript
let name = "John";
let age = 30;
console.log("Name: " + name);
console.log("Age: " + age);
```

上記のコードを実行すると、コンソールには以下のような出力が得られます。

```
Name: John
Age: 30
```

このように、`console.log()`メソッドを使用することで、変数の値を確認することができます。

## ディープダイブ

デバッグ出力をプリントする際には、変数の値だけでなく、コードの特定の箇所での動作を確認することもできます。例えば、条件分岐の処理を確認したい場合は、以下のようにコードを書くことができます。

```Javascript
let number = 10;
if (number > 5) {
  console.log("Number is greater than 5.");
} else {
  console.log("Number is less than or equal to 5.");
}
```

これにより、条件分岐の結果を確認することができます。

```
Number is greater than 5.
```

また、デバッグ出力をプリントすることで、コードのどの部分が実行されているのかを把握することもできます。これにより、どのようなロジックでプログラムが動作しているのかを理解することができます。

## 参考リンク

- [console.log()メソッドのドキュメント | MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/API/Console/log)
- [console.log():デバッグモード中の変数の表示 | Codecademy（英語）](https://www.codecademy.com/articles/console-log)
- [デバッグ出力 | Codecademy（英語）](https://www.codecademy.com/articles/debugging-output)