---
title:                "コンピュータプログラミングをするという記事のタイトルです: コマンドライン引数の読み取り"
html_title:           "Javascript: コンピュータプログラミングをするという記事のタイトルです: コマンドライン引数の読み取り"
simple_title:         "コンピュータプログラミングをするという記事のタイトルです: コマンドライン引数の読み取り"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読み取ることについて学ぶことは、JavaScriptの開発において非常に有用です。コマンドライン引数を利用することで、プログラムの実行時に外部から値を受け取ることができ、柔軟なプログラミングが可能になります。

## 方法

下記のようなコードを使用して、コマンドライン引数を読み取ることができます。

```Javascript
// コマンドライン引数を配列として取得
const args = process.argv.slice(2);

// 配列argsの要素を順に取り出し、コンソールに出力
args.forEach(arg => console.log(arg));
```

実行例：

```
node app.js hello world

// 出力結果
hello
world
```

コマンドライン引数は、プログラムの実行時に与えられる値を取得するためのものです。上記の例では、`hello`と`world`という値がコンソールに出力されます。また、`process.argv`を使用することで、引数以外の情報も取得することができます。

## ディープダイブ

コマンドライン引数は、主にコマンドラインから実行されるツールやプログラムの設定やオプションを指定するために使用されます。例えば、ブラウザで動作するJavaScriptを開発する際に、コマンドライン引数を利用してポート番号やホスト名を指定することができます。

また、Node.jsを使用してサーバーサイドのJavaScriptを実行する場合にも、コマンドライン引数を利用してプログラムの実行時の動作を制御することができます。

## 関連リンク

- [Node.jsドキュメント](https://nodejs.org/dist/latest-v14.x/docs/api/process.html#process_process_argv)
- [コマンドライン引数を読み取る方法](https://nodejs.org/en/knowledge/command-line/how-to-parse-command-line-arguments/)
- [コマンドライン引数について | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Infinity)