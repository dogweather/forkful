---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:20.313693-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.848764-06:00'
model: gpt-4-0125-preview
summary: "Rust\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\u304F\
  \u3068\u3044\u3046\u306E\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\
  \u4E0A\u306E\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3057\u3001\u66F8\u304D\u8FBC\
  \u307F\u3001\u305D\u3057\u3066\u53EF\u80FD\u3067\u3042\u308C\u3070\u30C7\u30FC\u30BF\
  \u3092\u8FFD\u52A0\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u30ED\u30B0\u3001\u8A2D\u5B9A\u3001\u307E\u305F\u306F\u30E6\u30FC\u30B6\u30FC\
  \u751F\u6210\u30B3\u30F3\u30C6\u30F3\u30C4\u306E\u3088\u3046\u306A\u30C7\u30FC\u30BF\
  \u3092\u6C38\u7D9A\u3055\u305B\u308B\u305F\u3081\u306B\u3053\u306E\u64CD\u4F5C\u3092\
  \u884C\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u306E\u5B9F\u884C\u7BC4\u56F2\u3092\u8D85\u3048\u3066\u30C7\u30FC\u30BF\u306E\u8010\
  \u4E45\u6027\u3092\u4FDD\u8A3C\u3057\u307E\u3059\u3002."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

## 何を、なぜ？
Rustでテキストファイルを書くというのは、ファイルシステム上のファイルを作成し、書き込み、そして可能であればデータを追加することを含みます。プログラマーは、アプリケーションログ、設定、またはユーザー生成コンテンツのようなデータを永続させるためにこの操作を行います。これは、プログラムの実行範囲を超えてデータの耐久性を保証します。

## どのように：
Rustの標準ライブラリは、主に`std::fs`と`std::io`モジュール内にカプセル化された、ファイル操作のための堅牢なツールを提供します。こちらはテキストファイルを作成して書き込む基本的な例です：

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("hello.txt")?;
    file.write_all(b"Hello, world!")?;
    Ok(())
}
```

このコードを実行した後、「hello.txt」という名前のファイルに"Hello, world!"という内容が書かれています。

より複雑なシナリオ、例えばファイルに追記する場合や、大量のデータを効率的に扱う場合には、Rustは追加の機能を提供します。既存のファイルにテキストを追加する方法はこちらです：

```rust
use std::fs::OpenOptions;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .open("hello.txt")?;
        
    file.write_all(b" Adding more text.")?;
    Ok(())
}
```

これを実行すると、「hello.txt」の末尾に" Adding more text."が追加されます。

場合によっては、サードパーティのライブラリを利用することで、ファイル操作を簡素化できます。たとえば、`serde`クレートと`serde_json`を組み合わせると、JSON形式へのデータ構造のシリアライズ（直列化）およびデシリアライズ（逆シリアル化）を行い、ファイルへの書き込みを高レベルで扱うことができます：

```rust
use serde::{Serialize, Deserialize};
use serde_json;
use std::fs::File;

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
}

fn main() -> std::io::Result<()> {
    let user = User { id: 1, name: "Jane Doe".into() };
    let file = File::create("user.json")?;
    serde_json::to_writer(file, &user)?;
    Ok(())
}
```

上記のコードを実行した後、`user.json`には`User`構造体のJSON表現が含まれます。`serde`と`serde_json`を使用するには、これらのクレートを`Cargo.toml`に追加する必要があります。

標準ライブラリを介して、または外部クレートの助けを借りてRustでテキストファイルを書くことは、アプリケーションにおけるデータの永続性を管理するための直感的かつ強力な方法です。
