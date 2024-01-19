---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なんで手に入れる？ どうして読む？

コマンドライン引数は、コードの実行時にプログラムに渡される情報です。プログラマは、この方法で外部からパラメーターを取得することで、一般的にコードの柔軟性と再利用性を向上させます。

## こうやって読む:

TypeScriptでコマンドライン引数を読むためには、組み込みの`process`オブジェクトを使います。以下に示すように`argv`プロパティを利用するだけです。

```TypeScript
let myArgs = process.argv.slice(2);
console.log(myArgs);
```
この例では、`myArgs`はコマンドライン引数の配列になります。ただし、最初の２つの引数はノードの実行経路とスクリプトファイルの経路のためには使わず、`slice(2)`で取り除きます。

## 深掘り

コマンドライン引数は、コンピュータが主流になる前から使われてきました。UNIXやLinuxのようなシステムでは、これらの引数はしばしばユーティリティの設定や振る舞いを制御するために使われてきました。

他に引数を処理する方法としては、ライブラリを使うこともあります。例えばノードパッケージ `yargs`や`commander`は、コマンドライン引数の解析と処理を助ける多くの強力な機能を提供します。

先述の例では、`process.argv`は全ての引数を文字列として返します。数値や真偽値を想定している場合、適切にキャストする必要があります。

## これも読んでみよう:

1. Node.jsのドキュメンテーション (`process.argv`について): https://nodejs.org/docs/latest/api/process.html#process_process_argv 
2. yargsパッケージ: https://www.npmjs.com/package/yargs
3. commanderパッケージ: https://www.npmjs.com/package/commander