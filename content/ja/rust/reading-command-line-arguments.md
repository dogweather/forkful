---
title:                "コマンドライン引数の読み取り"
aliases:
- ja/rust/reading-command-line-arguments.md
date:                  2024-01-20T17:56:57.351311-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

コマンドライン引数読み取りは、プログラム実行時にユーザーからの入力を受け取る方法です。これにより、汎用性が高まり、さまざまな状況やタスクに柔軟に対応できます。

## How to: (やり方)

Rustでコマンドライン引数を読み取る基本的なコードはシンプルです。

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
```

このコードを実行すると、以下のような出力になります。(実行例: `cargo run hello world`)

```Rust
["path/to/executable", "hello", "world"]
```

## Deep Dive (深い潜在)

### 歴史的背景
コマンドライン引数の読み取りはUNIX時代からの伝統的な方法です。Rustでは、`std::env`モジュールを使ってこの機能を提供します。

### 代替案
他のライブラリ、例えば`clap`や`structopt`を使うことで、より複雑なコマンドライン解析を行うことができます。これらはオプションのパーシング、サブコマンドの処理、自動的なヘルプメッセージの生成などの機能を提供します。

### 実装の詳細
`std::env::args`はイテレータを返します。これは遅延評価で、要求されたときにのみ引数を取得します。`collect()`メソッドを使って、イテレータからベクタを作ることができます。

## See Also (関連情報)

- 公式ドキュメント: [std::env](https://doc.rust-lang.org/std/env/)
