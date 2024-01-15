---
title:                "テキストファイルの書き方"
html_title:           "Rust: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

書き込み可能なテキストファイルを作成する理由は、情報を保存するためです。テキストファイルは、プログラムの設定ファイルからユーザーの入力まで、さまざまな用途に利用されます。

## How To

まず、新しいファイルを作成する必要があります。 `File` 構造体の `create` メソッドを使用して、ファイルを作成し、`write` メソッドを使用してデータを書き込むことができます。以下は、`hello.txt` という名前のファイルに "Hello, world!" というテキストを書き込む例です。

```Rust
use std::io::Write;
use std::fs::File;

fn main() {
    let mut file = File::create("hello.txt").expect("Cannot create file");
    file.write(b"Hello, world!").expect("Cannot write to file");
}
```

このコードを実行すると、現在のディレクトリに `hello.txt` ファイルが作成され、"Hello, world!" というテキストが書き込まれます。

## Deep Dive

Rustでは、標準ライブラリの `std::io` モジュールを使用してファイルの読み書きを行います。`File` 構造体には、ファイルの読み書きに使用できるさまざまなメソッドが用意されています。また、エラーハンドリングも重要な要素で、`expect` メソッドを使用することでエラーが発生した場合にメッセージを表示することができます。

また、テキストファイル以外にも、`File` 構造体を使用してバイナリファイルの読み書きも可能です。詳細な情報は公式ドキュメントを参照してください。

## See Also

- [std::fs::File - Rust Documentation](https://doc.rust-lang.org/std/fs/struct.File.html)
- [std::io::Write - Rust Documentation](https://doc.rust-lang.org/std/io/trait.Write.html)
- [Text File - Wikipedia](https://en.wikipedia.org/wiki/Text_file)