---
title:                "テキストファイルの読み込み"
aliases:
- /ja/rust/reading-a-text-file.md
date:                  2024-01-20T17:55:15.618154-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

テキストファイルを読むことは、ファイルの内容をプログラムで利用可能にする行為です。データの取り込み、設定の読込み、あるいは単に情報の表示が必要な場合などがあります。

## How to: (方法)

Rust では `std::fs` モジュールを使ってテキストファイルを簡単に読むことができます。以下に基本的な例を示します。

```rust
use std::fs;

fn main() {
    let filename = "greetings.txt";

    // ファイル内容を読み込み
    let contents = fs::read_to_string(filename)
        .expect("ファイルが見つからないか読めません");

    // 内容を出力
    println!("ファイルの内容:\n{}", contents);
}
```

これを実行すると、`greetings.txt` の内容がコンソールに表示されます。

## Deep Dive (深掘り)

ファイルの読み込みは I/O 操作の基本で、多くのプログラミング言語における標準的な機能です。Rust でも `std::fs::File` と `std::io::{Read, BufReader}` を使ってストリームからの読み込みやより高度な操作を行うことができます。状況に応じて `std::io::Error` の取り扱いには注意が必要です。また、非同期 I/O を扱うための `async-std` や `tokio` といったクレートが存在します。

ここで触れた `read_to_string` 関数は、ファイルのサイズが小さいときに便利ですが、大きなファイルの場合にはメモリの無駄遣いになることがあります。そのような場合には `BufReader` を使用することでメモリ効率を改善することができます。

## See Also (関連情報)

- Rust プログラミング言語の公式ドキュメント: https://doc.rust-lang.org/std/fs/index.html
- Rust by Example でのファイル I/O: https://doc.rust-lang.org/rust-by-example/std_misc/file.html
- `BufReader`: https://doc.rust-lang.org/std/io/struct.BufReader.html
- Async I/O in Rust with `tokio`: https://tokio.rs/
- Error handling in Rust: https://doc.rust-lang.org/book/ch09-00-error-handling.html
