---
title:                "「標準エラーへの書き込み」"
html_title:           "Rust: 「標準エラーへの書き込み」"
simple_title:         "「標準エラーへの書き込み」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

標準エラーへの書き込みを行う理由は、デバッグやエラーの追跡に役立つことです。

## 方法

### 準備をする
まず、標準エラーへの書き込みを行う準備をします。以下のコードを使用することで、標準エラーへのアクセスを許可します。

```Rust
use std::io::{self, Write};
```

### 標準エラーへの書き込み
標準エラーへの書き込みは、標準出力と同様に行うことができます。しかし、コードブロックの前に```eprintln!```を使用し、出力を標準エラーにリダイレクトします。

```Rust
eprintln!("This message will be written to standard error!");
```

上記のコードでは、```eprintln!```マクロを使用して、標準エラーへの書き込みを行っています。

### 実行結果
標準エラーへの書き込みを行うと、標準出力とは異なる場所にメッセージが表示されます。実際にプログラムを実行して、出力を確認してみましょう。

```Rust
eprintln!("This message will be written to standard error!");
println!("This message will be written to standard output!");
```

実行結果は以下のようになります。

```
This message will be written to standard output!
This message will be written to standard error!
```

## ディープダイブ

標準エラーへの書き込みは、プログラムをデバッグする際に非常に役立ちます。標準エラーは、標準出力とは異なる場所にメッセージを表示するため、デバッグメッセージを分離することができます。また、標準エラーはプログラムがクラッシュした際にも出力されるため、エラーの追跡にも役立ちます。

## 他でも読む
- [Rustの標準エラーへの書き込みについて](https://doc.rust-lang.org/std/macro.eprintln.html)
- [標準出力と標準エラーの違いを理解する](https://stackoverflow.com/questions/2342826/how-can-i-print-to-stderr-in-rust)