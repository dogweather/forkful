---
title:                "ディレクトリが存在するかどうかをチェックする"
html_title:           "Rust: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

ディレクトリが存在するかどうか確認する理由は一つだけではありません。プログラミングの世界では、特定の処理を行う前に必要な環境やファイルの存在を確認することが重要です。それが、ディレクトリの存在をチェックする理由です。また、ディレクトリが存在しない場合には、プログラムがエラーをキャッチして処理を中断することもできます。

## How To

ディレクトリが存在するかどうかを確認する方法は簡単です。まずは、```std::fs```モジュールをインポートします。そして、```Path::new()```メソッドを使用して、チェックしたいディレクトリのパスを指定します。最後に、```exists()```メソッドを呼び出して、ディレクトリの存在を確認します。以下は、そのコード例です。

```Rust
use std::fs;

let path = Path::new("/path/to/directory");

if path.exists() {
    println!("Directory exists!");
} else {
    println!("Directory does not exist.");
}
```

もしディレクトリが存在する場合は、"Directory exists!"というメッセージが表示されます。存在しない場合は、"Directory does not exist."というメッセージが表示されます。

## Deep Dive

もし、プログラムで複数のディレクトリを作成する場合や、既存のディレクトリを上書きする可能性がある場合は、```create_dir()```や```create_dir_all()```メソッドを使用することができます。これらのメソッドを使用することで、ディレクトリが存在しなければ自動的に作成されます。

また、上記のコード例ではパスを```Path::new()```メソッドで指定しましたが、ファイルシステム上の相対パスや絶対パスを使用することもできます。さらに、ディレクトリ以外にもファイルの存在をチェックする場合は、```is_file()```や```is_dir()```メソッドを使用することもできます。

## See Also

- [Path - Rust Documentation](https://doc.rust-lang.org/std/path/struct.Path.html)
- [fs::PathExt - Rust Documentation](https://doc.rust-lang.org/std/fs/trait.PathExt.html)
- [Check File And Directory Existence In Rust](https://www.aidangeorge.com/how-to-check-file-and-directory-existence-in-rust/)