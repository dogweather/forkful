---
title:                "コマンドライン引数の読み取り"
html_title:           "Rust: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読み取る必要性は、プログラムが外部から受け取る情報に依存している場合や、ユーザーがプログラムの挙動をカスタマイズしたい場合に特に重要です。

## 使い方

```Rust
use std::env;

fn main() {
    // コマンドライン引数を取得
    let args: Vec<String> = env::args().collect();
    
    // 引数がある場合、第1引数を表示
    if args.len() > 1 {
        println!("Hello, {}!", args[1]);
    } else {
        println!("Hello, world!");
    }
}
```

プログラムを `hello.rs` というファイル名で保存し、コンパイルした後、ターミナルで `./hello Rust` と入力すると、`Hello, Rust!` という出力が得られます。

## 深堀り

コマンドライン引数を扱う際、重要なことは引数の型と個数を把握することです。Rustでは、`std::env` モジュールの `args` 関数を使用して引数を取得し、 `len` メソッドを使用して引数の個数を判断することができます。また、引数の値は `String` 型で扱われるため、必要に応じて `parse` メソッドを使用して数値型に変換することもできます。

## さらに見る

- [Rust: Command line arguments](https://doc.rust-lang.org/std/env/fn.args.html)
- [Rust: Parsing command line arguments](https://doc.rust-lang.org/std/primitive.str.html#method.parse)
- [Rust: Types and variables](https://doc.rust-lang.org/book/ch03-01-variables-and-mutability.html)