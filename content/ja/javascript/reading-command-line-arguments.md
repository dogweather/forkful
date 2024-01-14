---
title:    "Javascript: コンピュータプログラミングの記事タイトル：コマンドライン引数の読み取り"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## なぜ

Javascriptでcommand line argumentsを読むべきか？プログラミングの問題を解決するために、より効率的で柔軟なコーディングを行うために、コマンドライン引数を読む必要があります。

## 方法

以下のコードを使用して、Javascriptでコマンドライン引数を読み取ることができます。


```Javascript
// コマンドライン引数を変数に代入する方法
var args = process.argv;

// 最初の引数はノードのパスなので、2番目の引数から値を取得する
var input1 = args[2];
var input2 = args[3];
var input3 = args[4];

// 取得した値をコンソールに出力する方法
console.log("入力1：", input1);
console.log("入力2：", input2);
console.log("入力3：", input3);
```

以下のようなコマンドを実行すると、引数として渡した値をコンソールに出力することができます。

```sh
node commandline.js apple orange banana
```

出力結果：

```sh
入力1： apple
入力2： orange
入力3： banana
```

## ディープダイブ

コマンドライン引数を読むことにはさらに多くの複雑なアプリケーションがあります。例えば、条件分岐やループを使用して、ユーザーが入力した引数に応じて異なる処理を実行することができます。

また、ユーザーが入力した引数を数値やデータ型に変換する必要がある場合もあります。これを行うためには、`parseInt()`や`parseFloat()`などの関数を使用することができます。

引数の数に制限を設けることもできます。これにより、ユーザーが特定の個数の引数を必ず入力するように強制することができます。

## See Also

- [Node.js Documentation on Process Object](https://nodejs.org/api/process.html)
- [W3Schools - Command Line Arguments in Node.js](https://www.w3schools.com/nodejs/nodejs_cmd_line.asp)
- [Mastering Node.js Command-line Interfaces - Towards Data Science](https://towardsdatascience.com/mastering-node-js-command-line-interfaces-28f7c6f75307)