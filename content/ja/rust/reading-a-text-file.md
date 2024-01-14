---
title:    "Rust: 「テキストファイルを読む」"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ読むのか

読み込み処理は、プログラミング言語を学ぶ上で重要な部分です。テキストファイルを読み込むことで、データを取得し、処理することができます。

## 方法

Rustでは、組み込みの`std::fs`ライブラリを使用して、テキストファイルを読み込むことができます。以下のコードを使用することで、簡単に実装することができます。

```Rust
use std::fs::File;
use std::io::prelude::*;
fn main() {
    let mut file = File::open("sample.txt").expect("ファイルを開くことができませんでした。");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("ファイルの読み込みに失敗しました。");
    println!("{}", contents);
}
```

上記のコードでは、`File::open`メソッドを使用して、`sample.txt`という名前のファイルを開き、その内容を`String`型の変数`contents`に読み込んでいます。そして、`println!`マクロを使用して、ファイルの中身を出力しています。

## ディープダイブ

テキストファイルを読み込む際、より詳細な制御が必要な場合があります。そのような場合には、`std::fs`ライブラリの`File`構造体を使用します。

`File`構造体の`open`メソッドを使用することで、ファイルを開く際のさまざまなオプションを指定することができます。また、ファイルの読み込みをストリーム形式で行うことも可能です。

詳細な情報は、Rustの公式ドキュメントを参照してください。

## 参考リンク

- [Rust公式ドキュメント](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rustにおけるファイル入出力](https://doc.rust-jp.rs/the-rust-programming-language-ja/1.6/std/fs/struct.File.html)
- [Rustでファイルを読み書きする方法](https://qiita.com/hdais/items/2da13ed2c12d854a4663)