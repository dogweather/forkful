---
date: 2024-01-20 17:55:15.618154-07:00
description: "How to: (\u65B9\u6CD5) Rust \u3067\u306F `std::fs` \u30E2\u30B8\u30E5\
  \u30FC\u30EB\u3092\u4F7F\u3063\u3066\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\
  \u3092\u7C21\u5358\u306B\u8AAD\u3080\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\
  \u4EE5\u4E0B\u306B\u57FA\u672C\u7684\u306A\u4F8B\u3092\u793A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.745549-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Rust \u3067\u306F `std::fs` \u30E2\u30B8\u30E5\u30FC\u30EB\
  \u3092\u4F7F\u3063\u3066\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u7C21\
  \u5358\u306B\u8AAD\u3080\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\
  \u306B\u57FA\u672C\u7684\u306A\u4F8B\u3092\u793A\u3057\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

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
