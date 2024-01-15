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

## なぜ
テキストファイルを読み込む理由はさまざまです。例えば、ファイル内のデータを解析したり、ファイルの内容を変更したりするプログラムを作成する場合には、まずはファイルを読み込んでそのデータを扱う必要があります。そこで、今回はRustを使ってテキストファイルを読み込む方法を紹介します。

## 使い方
まずは、ファイルを開いて読み込む準備をしましょう。Rustでは、`std::fs::File`モジュールを使用してファイルを開くことができます。次に、ファイル内のデータを読み込むために、`Read`トレイトを実装した`std::io::BufReader`を使用します。以下のコードを参考にしてみてください。

```Rust
use std::io::BufReader;
use std::io::prelude::*;
use std::fs::File;

fn main() {
    // ファイルを開き、データを読み込むための変数を定義する
    let file = File::open("test.txt").expect("ファイルが見つかりません");
    let mut reader = BufReader::new(file);
    
    // ファイル内のデータを読み込む
    let mut content = String::new();
    reader.read_to_string(&mut content).expect("データを読み込めません");
    
    // データを表示する
    println!("{}", content);
}
```

上記の例では、ファイルを開いてからデータを読み込むまでの基本的な流れを紹介しました。`BufReader`を使用することで、ファイルから読み込んだデータを一度に取得し、`String`型の変数に保存することができます。また、`String::new()`を使用することで、新しい空の`String`型の変数を定義することができます。

## 深堀り
テキストファイルを読み込む際には、ファイルの形式やエンコーディングなどにも気を配る必要があります。例えば、日本語を含むテキストファイルを読み込む場合には、`BufReader`のコンストラクタの引数として`Encoding`を指定する必要があります。また、エンドラインや空白など、特定の文字を無視してデータを読み込む際には、`skip`メソッドを使用することもできます。詳しくは公式ドキュメントを参照してみてください。

## 併せて読みたい
- [外部ファイルを読み書きする - Rustプログラミング言語](https://doc.rust-jp.rs/stable/book/second-edition/ch12-00-an-io-project.html)
- [RustにおけるファイルI/O - 基礎編 - Qiita](https://qiita.com/Yukihito/items/325b5c7d03287aed73c0)