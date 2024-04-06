---
date: 2024-01-20 17:56:57.351311-07:00
description: "How to: (\u3084\u308A\u65B9) Rust\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\
  \u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u307F\u53D6\u308B\u57FA\u672C\u7684\u306A\u30B3\
  \u30FC\u30C9\u306F\u30B7\u30F3\u30D7\u30EB\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.743145-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Rust\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\
  \u5F15\u6570\u3092\u8AAD\u307F\u53D6\u308B\u57FA\u672C\u7684\u306A\u30B3\u30FC\u30C9\
  \u306F\u30B7\u30F3\u30D7\u30EB\u3067\u3059\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
