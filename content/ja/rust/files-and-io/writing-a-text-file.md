---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:20.313693-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A Rust\u306E\u6A19\u6E96\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u306F\u3001\u4E3B\u306B`std::fs`\u3068`std::io`\u30E2\u30B8\u30E5\
  \u30FC\u30EB\u5185\u306B\u30AB\u30D7\u30BB\u30EB\u5316\u3055\u308C\u305F\u3001\u30D5\
  \u30A1\u30A4\u30EB\u64CD\u4F5C\u306E\u305F\u3081\u306E\u5805\u7262\u306A\u30C4\u30FC\
  \u30EB\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u3053\u3061\u3089\u306F\u30C6\u30AD\
  \u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3057\u3066\u66F8\u304D\u8FBC\
  \u3080\u57FA\u672C\u7684\u306A\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.746949-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

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
