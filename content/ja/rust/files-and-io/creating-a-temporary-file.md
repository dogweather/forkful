---
date: 2024-01-20 17:41:12.393479-07:00
description: "How to: Rust\u3067\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\
  \u3059\u308B\u306B\u306F\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306E`tempfile`\u30AF\
  \u30EC\u30FC\u30C8\u3092\u4F7F\u3046\u306E\u304C\u4E00\u822C\u7684\u3067\u3059\u3002\
  \u4EE5\u4E0B\u306F\u57FA\u672C\u7684\u306A\u4F7F\u3044\u65B9\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.850135-06:00'
model: gpt-4-1106-preview
summary: "Rust\u3067\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3059\u308B\
  \u306B\u306F\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306E`tempfile`\u30AF\u30EC\
  \u30FC\u30C8\u3092\u4F7F\u3046\u306E\u304C\u4E00\u822C\u7684\u3067\u3059\u3002\u4EE5\
  \u4E0B\u306F\u57FA\u672C\u7684\u306A\u4F7F\u3044\u65B9\u3067\u3059."
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

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
