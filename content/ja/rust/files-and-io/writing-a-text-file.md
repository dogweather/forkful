---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:20.313693-07:00
description: "\u2026"
lastmod: '2024-02-25T18:49:39.895974-07:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

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
