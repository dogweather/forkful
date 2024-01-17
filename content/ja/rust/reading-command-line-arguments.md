---
title:                "コマンドライン引数の読み込み"
html_title:           "Rust: コマンドライン引数の読み込み"
simple_title:         "コマンドライン引数の読み込み"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何をするのか？ 
コマンドライン引数の読み取りとは、プログラマーがプログラムを実行する際にコマンドラインから入力された値を取得することです。プログラマーは、ユーザーからの入力によってプログラムの挙動を変えたり、パラメーターを設定するために、コマンドライン引数を使用します。

## 方法： 
Rustでコマンドライン引数を読み取る方法は簡単です。下記のコードを使用して、```cargo run``` コマンドを実行することで、コマンドライン引数をプログラムに渡すことができます。

```
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("コマンドライン引数は {} です", args[1]);
}
```

コマンドライン引数として「Hello World」と入力された場合、プログラムは ```コマンドライン引数は Hello World です```と出力します。

## 詳しく見ていく 
コマンドライン引数を読み取る機能は、プログラミング言語やプログラムによって異なります。Rustでは、標準ライブラリの ```env``` モジュールを使用して、コマンドライン引数を取得することができます。また、ライブラリやフレームワークを使用することで、より細かいオプションやユーティリティ機能を提供することもできます。

## 参考情報 
Rustの ```env``` モジュールの詳細や使用方法については、公式ドキュメントを参照してください。また、他のプログラミング言語でも同様の機能を提供していることがありますので、興味があれば調べてみてください。

公式ドキュメント：https://doc.rust-lang.org/std/env/index.html