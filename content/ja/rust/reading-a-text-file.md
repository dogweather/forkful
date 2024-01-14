---
title:                "Rust: テキストファイルの読み込み"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読み込むという作業は、プログラミングにおいて非常に一般的であり、データ処理やファイル操作など多くのタスクに必要です。Rust言語を学ぶ上で、テキストファイルの読み込みは重要なスキルの一つです。この記事では、Rust言語を使用してテキストファイルを読み込む方法を紹介します。

## やり方

Rust言語を使用してテキストファイルを読み込むには、まずファイルを開く必要があります。ファイルを開くには、`File::open()`メソッドを使用します。例えば、以下のようにコードを書くことで、"sample.txt"というファイルを開くことができます。

```Rust
use std::fs::File;
use std::io::Read;

fn main() {
    let mut file = File::open("sample.txt").expect("ファイルが開けませんでした。");

    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("ファイルを読み込めませんでした。");

    println!("{}", contents);
}
```

このコードでは、`File::open()`メソッドの返り値を`file`変数に格納し、`file.read_to_string()`メソッドを使用してファイルを読み込んでいます。読み込んだテキストは、`contents`変数に格納されているので、`println!()`マクロを使用してコンソールに出力します。

もちろん、他にもファイルを読み込む方法はあります。例えば、バイト列としてファイルを読み込みたい場合は、`read()`メソッドを使用することもできます。また、ファイルの一部分だけを読み込む場合は、`seek()`および`read_exact()`メソッドを使用することもできます。

## 深堀り

ファイルを読み込む際に気をつけるべきポイントとして、エラーハンドリングが挙げられます。ファイルを開けない場合や読み込めない場合、パスが間違っていた場合など、予期しないエラーが発生する可能性があります。そのため、ファイルの開き方や読み込み方を正しく理解し、エラー処理を適切に行うようにしましょう。

また、Rust言語では`std::fs`モジュールと`std::io`モジュールを使用することで、ファイル操作やデータの入出力を行うことができます。これらのモジュールについてもしっかりと学ぶことで、より高度なテキストファイルの処理が可能になるでしょう。

## その他の参考リンク

- [Rustでファイルを開く方法](https://doc.rust-lang.org/std/fs/struct.File.html#method.open)
- [Rustでファイルからテキストを読み込む方法](https://doc.rust-lang.org/std/fs/struct.File.html#method.read_to_string)
- [Rustのioモジュールのドキュメント](https://doc.rust-lang.org/std/io/index.html)