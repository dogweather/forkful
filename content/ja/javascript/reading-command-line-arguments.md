---
title:                "Javascript: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読み取ることは、プログラムをより柔軟にするために非常に重要です。特定のパラメーターを指定することで、プログラムの挙動を変更することができます。例えば、ユーザーがプログラムに与える入力の形式を変更することができます。

## 使い方

コマンドライン引数は、プログラムの起動時に与えられるパラメーターです。コマンドライン引数を読み取るには、`process.argv`という特殊なオブジェクトを使用します。以下のコードは、コマンドライン引数を出力する簡単なプログラムの例です。

```Javascript
// process.argvの中身を出力する
console.log(process.argv);
```

実際にプログラムを実行してみると、以下のように出力されるでしょう。

```
$ node index.js hello world
[ '/usr/local/bin/node', '/Users/user/Desktop/index.js', 'hello', 'world' ]
```

この例では、`node`コマンドを使用して`index.js`ファイルを実行し、`hello`と`world`という2つの引数を与えています。`process.argv`には、最初に`node`コマンドのパス、次に実行しているファイルのパス、そして与えた引数が順番に格納されています。

## 深堀り

もっと詳しく説明すると、`process.argv`は実は配列であることがわかります。なので、配列のメソッドを使用することでコマンドライン引数をより柔軟に扱うことができます。例えば、次のコードは与えられた引数の数を出力するプログラムです。

```Javascript
// process.argvの要素数を出力する
console.log(process.argv.length - 2);
```

実際に実行してみると、引数の数に応じて出力結果が変化します。また、`process.argv`の要素をループ処理することで、より高度なプログラムを作ることも可能です。

## See Also

- [Node.jsのドキュメント - process.argv](https://nodejs.org/api/process.html#process_process_argv)
- [コマンドライン引数を扱う方法（Qiita）](https://qiita.com/babie/items/db718a6d241b3966110c)