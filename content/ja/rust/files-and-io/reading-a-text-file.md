---
date: 2024-01-20 17:55:15.618154-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3053\
  \u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u3067\u5229\u7528\u53EF\u80FD\u306B\u3059\u308B\u884C\u70BA\u3067\u3059\
  \u3002\u30C7\u30FC\u30BF\u306E\u53D6\u308A\u8FBC\u307F\u3001\u8A2D\u5B9A\u306E\u8AAD\
  \u8FBC\u307F\u3001\u3042\u308B\u3044\u306F\u5358\u306B\u60C5\u5831\u306E\u8868\u793A\
  \u304C\u5FC5\u8981\u306A\u5834\u5408\u306A\u3069\u304C\u3042\u308A\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.847545-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3053\
  \u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u3067\u5229\u7528\u53EF\u80FD\u306B\u3059\u308B\u884C\u70BA\u3067\u3059\
  \u3002\u30C7\u30FC\u30BF\u306E\u53D6\u308A\u8FBC\u307F\u3001\u8A2D\u5B9A\u306E\u8AAD\
  \u8FBC\u307F\u3001\u3042\u308B\u3044\u306F\u5358\u306B\u60C5\u5831\u306E\u8868\u793A\
  \u304C\u5FC5\u8981\u306A\u5834\u5408\u306A\u3069\u304C\u3042\u308A\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
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
