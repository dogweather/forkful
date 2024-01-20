---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？
コマンドライン引数の読み取りは、ユーザーがコマンドラインからスクリプトに情報を渡す一般的な方法です。これにより、プログラムをより汎用的かつ対話的にすることができます。

## 方法：
Node.jsでコマンドライン引数を読み取るには、process.argvを使用します。以下にその例を示します。
```Javascript
// コード
console.log(process.argv);

// 出力例. 'node app.js one two=three'を実行した場合
['/usr/local/bin/node', '/Users/username/app.js', 'one', 'two=three']
```
この例ではprocess.argvが配列を返し、その各要素が1つのコマンドライン引数に対応しています。

## ディープダイブ：
コマンドライン引数の読み取りは、UNIXの初期から存在しています。これは、ユーザーが実行時にスクリプトに追加の情報を提供できるようにするためのものです。

代替としては、npmパッケージのminimistやcommanderなどがあります。これらは、コマンドライン引数のパーシング（解析）をより容易にし、柔軟性と高度な機能をもたらします。

実装の詳細については、process.argvは始まりにノードの実行パスとスクリプトのパスを含んでいます。実際のコマンドライン引数はインデックス2から開始します。

## 参考文献：
1. Node.jsのドキュメンテーション - [process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
2. npmパッケージ - [minimist](https://www.npmjs.com/package/minimist)
3. npmパッケージ - [commander](https://www.npmjs.com/package/commander)