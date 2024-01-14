---
title:    "Javascript: 「標準エラーに書き込む」"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## なぜ標準エラー出力を書くのか
プログラミングをする際には、バグを修正したり問題を解決したりするためにデバッグ情報が必要になることがあります。その際に、標準エラー出力を利用することでエラーをより詳細に把握し、プログラムの開発をスムーズに行うことができます。

## 方法
標準エラー出力を書く方法は簡単です。まずは```console.error()```を使用して、エラーメッセージを出力します。次に、```console.trace()```を使用してどの部分でエラーが発生したかを特定することができます。最後に、コンソールに出力された内容を確認して、必要に応じて修正ができます。

例えば、以下のコードを見てみましょう。

```Javascript
let num1 = 5;
let num2 = 'TWO';

if(typeof num1 === 'string'){
  console.error('num1 is not a number');
}

if(!Number.isNaN(num2)){
  console.trace('num2 is not a number');
}
```

上記のコードでは、```console.error()```と```console.trace()```を使用して、それぞれの変数のデータ型が数値であるかを確認しています。実行すると、コンソールに以下のような結果が出力されます。

```Javascript
num1 is not a number
Trace
    at Object.<anonymous> (filename:line:col)
    at Module._compile (module.js:678:30)
    at Object.Module._extensions..js (module.js:690:10)
    at Module.load (module.js:589:32)
    at tryModuleLoad (module.js:528:12)
    at Function.Module._load (module.js:520:3)
    at Function.Module.runMain (module.js:703:10)
    at startup (bootstrap_node.js:193:16)
    at bootstrap_node.js:618:3
```

このように、エラーメッセージとその内容の詳細が表示され、バグや問題の特定がしやすくなります。

## 詳細解説
より深く標準エラー出力について学ぶには、以下のリンクを参考にしてみてください。

- [Node.js Documentation: Console](https://nodejs.org/api/console.html)
- [JavaScript.info: Console](https://javascript.info/console)

## 参考リンク
- [Error Handling in JavaScript: A Beginner's Guide](https://stackify.com/error-handling-in-javascript/)
- [Debugging JavaScript: Advanced Techniques](https://www.telerik.com/blogs/advanced-javascript-debugging-techniques#:~:text=Lets%20Define%20the%20Debugger!,-Debugger%20is%20a)