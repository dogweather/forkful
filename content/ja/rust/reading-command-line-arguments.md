---
title:    "Rust: コンピュータープログラミングにおける「コマンドライン引数の読み込み」"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読み取ることが重要な理由は、プログラムに動的なデータを与えることができるためです。コマンドライン引数を使用すると、同じコードを使用してさまざまな入力を処理することができ、便利です。

## 使い方

Rustでは、[std::env](https://doc.rust-lang.org/std/env/index.html)ライブラリを使用して、コマンドライン引数を読み取ることができます。まず、`env::args()`を使用して引数をベクターとして取得します。その後、引数を文字列に変換して処理することができます。以下にコマンドライン引数を読み取るサンプルコードを示します。

```Rust
use std::env;

fn main() {
    // コマンドライン引数をベクターとして取得
    let args: Vec<String> = env::args().collect();

    // プログラム名を除いた引数を取得
    let message = &args[1];

    println!("あなたのメッセージは: {}", message);
}
```

このコードをコンパイルし、プログラム名とメッセージという2つの引数を与えると、メッセージが表示されます。

```sh
$ ./args プログラム名 メッセージ
あなたのメッセージは: メッセージ
```

## 深堀り

コマンドライン引数には、さまざまなタイプの引数があります。例えばオプション引数や数値引数などです。Rustでは、[StructOpt](https://docs.rs/structopt/0.3.18/structopt/)ライブラリを使用することで、簡単にこれらの引数を扱うことができます。また、コマンドライン引数のヘルプメッセージを自動的に生成することもできます。

また、コマンドライン引数の解析は[clap](https://docs.rs/structopt/0.3.18/structopt/clppy/)ライブラリを使用することでも可能です。このライブラリには、より詳細な解析機能やフロントエンドツールの開発に役立つ機能があります。

## 参考リンク

- [Rustのドキュメンテーション](https://doc.rust-jp.rs/book-ja/ch12-01-accepting-command-line-arguments.html)
- [std::envのドキュメンテーション](https://doc.rust-lang.org/std/env/)
- [StructOptのドキュメンテーション](https://docs.rs/structopt/)
- [clapのドキュメンテーション](https://docs.rs/clap/)