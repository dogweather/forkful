---
title:                "Rust: 「コマンドライン引数の読み込み」"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ？

コマンドライン引数の読み取りを学ぶことは、Rustプログラミングにとって非常に重要です。コマンドライン引数を正しく理解することで、より柔軟で効率的なプログラムを書くことができます。

## 方法

コマンドライン引数を読み取るには、標準ライブラリの`std::env`を使用します。まず、`use std::env`を使用してライブラリをインポートし、次に`std::env::args()`を使用してコマンドライン引数のイテレーターを取得します。このイテレーターを使用して、引数を一つずつ処理することができます。

例えば、コマンドライン引数で与えられた数値を合計するプログラムを書くとします。その場合、以下のようにコードを記述することができます。

```Rust
use std::env;
let mut sum = 0;

for arg in env::args().skip(1) {
    let num: i32 = arg.parse().unwrap(); // 引数を数値に変換
    sum += num; // 合計に加算
}

println!("The sum is {}", sum); // 結果を表示
```

コマンドラインで`rustc sum_args.rs`のようにファイル名を与えずに実行すると、エラーが発生することに注意しましょう。なぜなら、最初の引数にはファイル名が渡されているため、数値ではないからです。そのため、`.skip(1)`を使用してファイル名をスキップするようにしています。

## ディープダイブ

コマンドライン引数の詳細は非常に深いトピックです。例えば、オプション引数を使用したい場合は、`std::env::args()`ではなく`std::env::args_os()`を使用する必要があります。これにより、Unicodeでエンコードされた引数を処理することができます。

また、クレート（ライブラリ）を使用してより高度な引数のパースを行うこともできます。こういったトピックは、より詳細な学習や実践によって深めることができます。

## 参考リンク

- [Rust標準ライブラリドキュメント - envモジュール](https://doc.rust-lang.org/std/env/index.html)
- [Rust by Example - コマンドライン引数](https://doc.rust-lang.org/stable/rust-by-example/std_misc/arg.html)
- [clap - Rustのコマンドラインパーサーライブラリ](https://docs.rs/clap/2.33.0/clap/)