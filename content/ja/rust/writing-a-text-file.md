---
title:                "Rust: テキストファイルを作成する"
simple_title:         "テキストファイルを作成する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜテキストファイルを書くのか

テキストファイルを書くことの目的は、データや情報を保存、編集、共有するためです。これまでに開発されたプログラミング言語には、ファイルを編集するためのさまざまな機能やツールがありますが、Rustはその中でも優れた選択肢です。Rustの安全性と高速性のおかげで、テキストファイルの処理がより効率的に行えるようになりました。

## どのようにしてテキストファイルを書くのか

テキストファイルをRustで書くための基本的な方法を紹介します。まず、ファイルを操作するために`std::fs::File`モジュールを使用します。次に、`create`メソッドを使って新しいファイルを作成し、`write`メソッドを使ってデータをファイルに書き込みます。最後に、ファイルを閉じるために`close`メソッドを使用します。

```Rust
use std::fs::File;

fn main() {
    let mut file = File::create("sample.txt").expect("ファイルを作成できませんでした。");

    let content = b"こんにちは、世界！";
    file.write_all(content).expect("ファイルに書き込めませんでした。");
}
```

実行すると、`sample.txt`ファイルに`こんにちは、世界！`という文字列が書き込まれます。

## テキストファイルの詳細

テキストファイルをRustで書く場合、注意しなければいけないポイントがいくつかあります。例えば、ファイルへの書き込みの際にはエラー処理をきちんと行う必要があります。また、文字列をバイト列に変換する必要があります。さらに、テキストファイルの改行コードには注意が必要です。Rustでは、改行コードを明示的に指定しなければいけません。

## See Also

- [Rust公式ドキュメント - ファイルの操作](https://doc.rust-lang.org/std/fs/fn.File.html)
- [The Rust Programming Language](https://www.rust-lang.org/ja/)
- [Rustではじめるプログラミング入門](https://gihyo.jp/book/2020/978-4-297-10883-4)