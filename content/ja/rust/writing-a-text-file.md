---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"

category:             "Rust"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
テキストファイルの書き込みとは、データを永続的に保存するための一つの方法です。プログラマーは設定、記録、またはデータの交換を目的としてこれを行います。

## How to:

```Rust
use std::fs::File;
use std::io::Write;

fn main() -> std::io::Result<()> {
    let mut file = File::create("output.txt")?;
    file.write_all(b"こんにちは、Rustの世界！")?;
    Ok(())
}
```

サンプル出力: output.txtに「こんにちは、Rustの世界！」が書き込まれます。

## Deep Dive
Rustでのテキストファイル書き込みはシンプルですが、内部的にはOSのシステムコールを使い、バッファリング、エラー処理、文字エンコーディングなどの考慮を必要とします。`write!`や`writeln!`マクロを使うと、フォーマットされたテキストを書くこともできます。また、Rustは1.0リリース以来この機能を提供しており、安全性と効率のために多重の改良が行われてきました。

## See Also
- [Rust by Example](https://doc.rust-lang.org/stable/rust-by-example/std_misc/file.html)
- [Rust Documentation for std::fs](https://doc.rust-lang.org/std/fs/)
- [Rust Documentation for std::io](https://doc.rust-lang.org/std/io/)
