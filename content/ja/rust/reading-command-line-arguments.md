---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何そして、なぜ？
コマンドライン引数の読み取りは、プログラム起動時にユーザーからデータを受け取る手段です。プログラマーはコマンドラインオプションやファイル取り扱い、その他外部からインプットを受け取るために、これを使用します。

## やり方：
Rustでは、コマンドライン引数を読み取るために std::env::args() 関数を利用します。

```Rust
fn main() {
    let args: Vec<String> = std::env::args().collect();

    for arg in args {
        println!("{}", arg);
    }
}
```
これをコマンドラインで実行すると、引数は順序通りに印字されます。

```bash
> cargo run arg1 arg2 arg3
```

出力:

```bash
target/debug/rust_program
arg1
arg2
arg3
```

## ディープダイブ
1. コマンドライン引数の概念は、Unixオペレーティングシステムの初期から存在しています。この概念は、多くのモダンプログラミング言語、Rustを含む、に引き継がれました。

2. コマンドライン引数を読む為の代替手段として、ファイルや環境変数、ユーザーの直接のインタラクションなどが考えられます。

3. Rustでは、コマンドライン引数はオペレーティングシステムからプログラムへと伝達されます。他のプログラムと同様に、Rustプログラムも0番目の引数として自身の名前を受け取ります。

## 参考情報
1. Official Rust Documentation: std::env::args: https://doc.rust-lang.org/std/env/fn.args.html
2. The Rust Book: Command Line Programs: https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html
3. Command Line Arguments in Rust: https://stevedonovan.github.io/rustifications/2018/09/08/common-command-line-arguments-in-rust.html