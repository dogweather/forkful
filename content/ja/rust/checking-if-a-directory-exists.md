---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "Rust: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なに & なぜ？

ディレクトリが存在するかどうかをチェックすることは、プログラマーがファイルやデータにアクセスする前に、そのディレクトリが存在するかどうかを確認することです。これにより、エラーが発生する可能性を減らし、スムーズにプログラムを実行することができます。

## 方法：

```Rust
use std::path::Path;

let path = Path::new("directory_name");

if path.exists() {
    println!("Directory exists!");
} else {
    println!("Directory does not exist!");
}
```
上記の例では、Pathモジュールを使用してディレクトリのパスを作成し、exists()メソッドを使用してディレクトリが存在するかどうかを確認しています。もしディレクトリが存在すれば、「Directory exists!」というメッセージが表示され、存在しなければ「Directory does not exist!」というメッセージが表示されます。

## 深層掘り

ディレクトリの存在をチェックする方法は、プログラミング言語や環境によって異なります。Rustでは、Pathモジュールを使用してディレクトリのパスを作成し、exists()メソッドを使用することで簡単にチェックすることができます。他の言語では、システムコールを使用してディレクトリの存在をチェックする必要がある場合もあります。

## 関連情報

- [Std::path - Rust Documentation](https://doc.rust-lang.org/std/path/struct.Path.html)
- [Checking if a file exists using Rust - GeeksforGeeks](https://www.geeksforgeeks.org/checking-if-a-file-exists-using-rust/)