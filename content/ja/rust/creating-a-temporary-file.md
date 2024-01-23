---
title:                "一時ファイルの作成"
date:                  2024-01-20T17:41:12.393479-07:00
model:                 gpt-4-1106-preview
simple_title:         "一時ファイルの作成"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
一時ファイルって何？一時ファイルは一時的なデータ保管のために生成されるファイルです。プログラマがなぜ使うかというと、データを一時的に扱いたい場合、または他の処理で後で消す予定のファイルが必要な場合に重宝します。

## How to:
Rustで一時ファイルを作成するには標準ライブラリの`tempfile`クレートを使うのが一般的です。以下は基本的な使い方です。

```Rust
use tempfile::Builder;

fn create_temp_file() -> std::io::Result<()> {
    let mut temp_file = Builder::new().prefix("example").tempfile()?;
    writeln!(temp_file, "一時ファイルに書き込みます。")?;
    // 一時ファイルのパスを表示
    println!("{:?}", temp_file.path());
    Ok(())
}

fn main() {
    match create_temp_file() {
        Ok(_) => println!("一時ファイル作成成功！"),
        Err(e) => eprintln!("エラー発生: {}", e),
    }
}
```

サンプル出力:
```
一時ファイル作成成功！
"/tmp/exampleax4Q2z"
```

## Deep Dive
かつては一時ファイルを自作するのが一般的でしたが、ユニークなファイル名の衝突を避けたり、セキュリティを保つためには`tempfile`のようなライブラリが便利です。他の言語には似たようなライブラリがありますが、Rustでは`tempfile`がよく使われています。

`tempfile`の背後には、ファイル名の競合を防ぐためにランダムなファイル名を生成するロジックがあります。また、オプションでプレフィックス、サフィックスやカスタムのディレクトリパスを設定することも可能です。

このライブラリはOSレベルでの安全な一時ファイル生成機能を利用することで、セキュリティも確保しています。例えばUNIX系システムでは`mkstemp`システム呼び出しが背後で使われています。

## See Also
- [The tempfile crate documentation](https://docs.rs/tempfile/)
- [Rust by Example on File I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
- [Official Rust programming language book](https://doc.rust-lang.org/book/)
