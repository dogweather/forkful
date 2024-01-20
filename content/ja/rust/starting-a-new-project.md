---
title:                "新しいプロジェクトを始める"
html_title:           "C: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 何となぜ？

新プロジェクトの開始とは、新しいソフトウェア開発が始まることを意味します。プログラマーはこれを行うことで、アイデアを実現し、解決策を提供します。

## どのように行うか：

新しいプロジェクトを開始するために、Rustで利用可能な `cargo new` コマンドを使用します。

```Rust
$ cargo new my_project
```

上記のコマンド実行後、`my_project` ディレクトリが作成され、新しいRustプロジェクトの初期セットアップが行われます。

```Rust
$ ls my_project
Cargo.toml
src

$ cat my_project/Cargo.toml
[package]
name = "my_project"
version = "0.1.0"
authors = ["Your Name <you@example.com>"]
edition = "2018"

$ cat my_project/src/main.rs
fn main() {
    println!("Hello, world!");
}
```

## より深い情報：

Rustの `cargo new` コマンドは、新規プロジェクトに必要なファイルとディレクトリ構造を自動的に生成します。このコマンドはPythonの `venv` やJavaの `gradle init` などと同等です。

初期設定では、固定の "Hello, world!" プログラムが含まれていますが、これは開発者が自身のプログラムに置換できます。`Cargo.toml` はプロジェクトの設定ファイルで、プロジェクトに関する重要な情報、使用する依存関係やビルド設定などが記載されます。

## 参考情報：

- [公式Rustドキュメンテーション](https://www.rust-lang.org/learn)
- [Cargoのガイド](https://doc.rust-lang.org/cargo/)
- [Rustプログラミングの入門ガイド](https://doc.rust-jp.rs/book-ja/)