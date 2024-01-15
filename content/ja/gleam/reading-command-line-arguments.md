---
title:                "コマンドライン引数の読み取り"
html_title:           "Gleam: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読み取る方法を学ぶことで、あなたのプログラミングスキルを向上させることができます。また、コマンドライン引数を使うことで、プログラムをより動的に制御できるようになります。

## 方法

コマンドライン引数を読み取るには、Gleamの標準ライブラリに含まれている `gleam/io/argv` モジュールを使用します。以下のコードを参考にしてください。

```Gleam
import gleam/io/argv

pub fn main() {
  args = argv.args()               // 引数を取得する
  println(args)                    // 引数を表示する
  println(args[0])                 // 最初の引数を表示する
  println(args[1])                 // 2番目の引数を表示する
  println(args.len())              // 引数の数を表示する
}
```

もしあなたが `gleam build` コマンドでコンパイルしたプログラムを実行する場合は、 `gleam run my_program`` `と入力してください。もしもあなたが、 `gleam repl`` `でプログラムを実行する場合、コマンドライン引数は自動的に与えられます。

サンプルの出力は以下のようになります。

```Gleam
my_program
arg1
arg2
2
```

## 詳細

`argv.args()` 関数は、文字列のリストを返します。もしもコマンドライン引数を指定しなかった場合、この関数は空のリストを返します。`argv.args()` 関数で返されるリストは、配列に似ていますが、 `append`` ` や `split` などの配列の一部の関数を使用することはできません。そのため、 `args[0]`` ` のようなインデックスを使用する必要があります。

## 関連情報

- [Gleamのドキュメンテーション](https://gleam.run/documentation/)
- [コマンドライン引数の読み取りについての詳細](https://gleam.run/articles/command-line-arguments/)