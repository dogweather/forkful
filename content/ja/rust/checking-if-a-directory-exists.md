---
title:    "Rust: ディレクトリの存在をチェックする"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ
ディレクトリが存在するかどうかを確認する必要がある理由は、ファイルやデータを処理する際に、それらが存在するかどうかを事前に確認することができるためです。これにより、エラーを防ぐことができ、より効率的にプログラムを実行することができます。

## 方法
ディレクトリが存在するかどうかを確認する方法は、Rustの標準ライブラリで提供されている`fs`モジュールを使用することです。まず、`std::fs::metadata`関数を使用してディレクトリのメタデータを取得します。次に、`is_dir`メソッドを使用して、ディレクトリであるかどうかを確認します。

```Rust
use std::fs;

let dir_path = "/my/directory/path";

if let Ok(metadata) = fs::metadata(&dir_path) {
    if metadata.is_dir() {
        println!("The directory exists!");
    } else {
        println!("The given path is not a directory");
    }
} else {
    println!("The directory does not exist");
}
```

上記のコードでは、`fs::metadata`関数を使用して`dir_path`に指定したパスのメタデータを取得し、`is_dir`メソッドを使用してディレクトリかどうかを確認しています。もしディレクトリであれば、`The directory exists!`というメッセージが出力されます。

## ディープダイブ
ディレクトリが存在するかどうかを確認する際には、ファイルインライターのような、`fs::metadata`を使用する関数が便利です。これらの関数は、Rustプログラムで実行するファイルを取得したり、ファイルを作成、編集する際にも使用することができます。また、`is_dir`メソッドのほかにも、`is_file`メソッドを使用することでも、ファイルの存在を確認することができます。

## 関連リンク
- [Rustの標準ライブラリ](https://doc.rust-lang.org/std/fs/index.html)
- [ファイルとディレクトリを操作する方法](https://qiita.com/thara0214/items/55daf35d923ee4406006)