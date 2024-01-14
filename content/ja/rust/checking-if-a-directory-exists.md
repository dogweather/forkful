---
title:    "Rust: ディレクトリが存在するかどうかを確認する"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかをチェックする理由は、プログラムを実行する前に存在しないディレクトリを作成する必要があるかどうかを知るためです。

## 方法

ディレクトリが存在するかどうかを確認するには、Rustの標準ライブラリである`std::fs`を使用します。まず、ディレクトリのパスを`Path`型に変換します。次に、`Path`型の`exists()`メソッドを使用して、ディレクトリが存在するかどうかを確認します。最後に、`if`文を使用して、ディレクトリが存在する場合と存在しない場合の処理を分岐させます。

```Rust
use std::fs;
use std::path::Path;

fn main() {
    // ディレクトリのパスを定義
    let directory_path = Path::new("path/to/directory");

    // ディレクトリが存在するかどうかをチェック
    if directory_path.exists() {
        println!("ディレクトリが存在します。");
    } else {
        println!("ディレクトリが存在しません。");
    }
}
```

上記のコードを実行すると、ディレクトリが存在するかどうかに応じてメッセージが出力されます。

## ディープダイブ

`exists()`メソッドは、`std::path::Path`型のメソッドとして定義されています。このメソッドは、ファイルやディレクトリが存在するかどうかを確認するために使用できます。また、同じく`std::fs`に定義されている`create_dir()`メソッドを使用することで、ディレクトリを新しく作成することもできます。

## 関連記事

- [Rustプログラミング言語公式ウェブサイト](https://www.rust-lang.org/ja/)
- [Rustの標準ライブラリドキュメント](https://doc.rust-lang.org/std/index.html)
- [Rustでファイルの読み書きをする方法 - Qiita](https://qiita.com/LyoSU/items/2d5ff44adc6ff90e92ce)