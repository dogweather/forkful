---
title:                "デバッグ出力を印刷する"
html_title:           "Javascript: デバッグ出力を印刷する"
simple_title:         "デバッグ出力を印刷する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

デバッグ出力を表示するのにはどのような意義があるのか？個人的には、コードの実行過程を理解し、問題箇所を特定するためにとても便利です。また、デバッグ出力を活用することで、コードの挙動を視覚的に確認することも可能です。

## 方法

```Javascript
console.log('Hello, World!');
```

デバッグ出力を表示するには、`console.log()`を使用します。この関数は、引数として与えられた値をコンソールに出力します。上の例では、`'Hello, World!'`という文字列を出力します。

```Javascript
let num1 = 5;
let num2 = 10;
let sum = num1 + num2;
console.log(sum);
```

変数を使用することも可能です。上の例では、`num1`と`num2`という2つの数値を足した結果を、`sum`という変数に代入し、`console.log()`を使用して出力しています。

## 深堀り

デバッグ出力は、コードを実行する過程での値や変数の状態を確認するのに役立ちます。また、`console.log()`には、複数の引数を渡すことができ、それらをカンマで区切って出力できるようになっています。

```Javascript
let name = 'John';
let age = 30;
console.log('Name:', name, 'Age:', age);
```

上の例では、`name`と`age`という2つの変数を出力し、それぞれの前にラベルを付けています。これにより、より分かりやすいデバッグ出力が可能になります。

## 関連リンクを参照

- [MDN Web Docs - console](https://developer.mozilla.org/ja/docs/Web/API/Console) 
- [JavaScript.info - console, alert, prompt](https://javascript.info/alert-prompt-confirm)