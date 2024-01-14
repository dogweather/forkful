---
title:    "Javascript: コンピュータプログラミングの記事題: コマンドライン引数の読み取り"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

プログラムの実行時に、コマンドライン引数を読み取ることはプログラマーにとって非常に便利です。これにより、プログラムの処理を柔軟にカスタマイズすることができます。

## How To

プログラム内でコマンドライン引数を読み取るには、以下のようなコードを記述します。

```Javascript
// コマンドライン引数の配列を取得する
let args = process.argv;

// 最初の引数は常にNode.js実行ファイルのパスになるため、2番目の引数以降を使用する
let userInput = args.slice(2);

// 引数の数に応じて条件分岐を行う
if (userInput.length === 0) {
  console.log("引数がありません。");
} else {
  console.log("入力された引数は" + userInput + "です。");
}
```

そして、コマンドラインでプログラムを実行する際に引数を指定します。

```
node index.js 引数1 引数2
```

この場合、コンソールには「入力された引数は引数1, 引数2です。」と表示されます。

## Deep Dive

コマンドライン引数を読み取るには、process.argvというNode.jsの組み込み変数を使用します。この変数は、実行時に渡された引数の配列を保持しています。また、spliceを使用することで実行ファイルのパスを無視することもできます。

また、引数の数に応じて条件分岐を行うことで、さまざまなパターンに対応することができます。これにより、ユーザーが何らかの入力を行わなかった場合にもエラーを返すことができます。

## See Also

- [Node.js Documentation - process.argv](https://nodejs.org/api/process.html#process_process_argv)
- [How to Read Command Line Arguments in JavaScript](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-node-js)