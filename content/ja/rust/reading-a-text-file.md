---
title:                "テキストファイルの読み込み"
html_title:           "Rust: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なに&なぜ?
テキストファイルを読み取ることとは、プログラマーがあるファイルに書かれているテキストを取得することを指します。プログラマーがテキストファイルを読み取る理由は、そのテキストを処理することで、プログラムをより柔軟に作れるからです。

## 使い方:
```Rust
use std::fs;

fn main() {
    let file = fs::read_to_string("input.txt").expect("Failed to read file");
    println!("File content: \n{}", file);
}
```
上記のコードでは、fsライブラリを使ってテキストファイルを読み取り、その内容を表示しています。`expect`メソッドを使うことで、エラーがあった場合には指定したメッセージを表示するようにしています。

## 深く掘り下げる:
テキストファイルを読み取る方法は様々ありますが、Rustでは`std::fs`ライブラリの`read_to_string`メソッドを使うことが一般的です。他の方法としては、`std::io`ライブラリの`BufReader`を使ってファイルを行単位で読み取る方法があります。また、テキストファイルのエンコーディングにも注意が必要で、UTF-8であれば`to_string`メソッドを使って取得することができますが、それ以外のエンコーディングの場合は別の方法を使う必要があります。

## 関連リンク:
- [Rust Reference](https://doc.rust-lang.org/reference/) - テキストファイルを読み取る方法を詳しく説明しています。
- [Rust API Documentation](https://doc.rust-lang.org/std/fs/fn.read_to_string.html) - `read_to_string`メソッドの使い方や引数の意味を確認できます。
- [Rust Cookbook](https://rust-lang-nursery.github.io/rust-cookbook/file/read-write.html) - ファイル読み取りや書き込みに関する様々なレシピを掲載しています。