---
title:                "文字列を小文字に変換"
date:                  2024-01-20T17:39:07.479286-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

category:             "Rust"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
## 何となぜ?
文字列を小文字にするとは、全てのアルファベットを小文字版に変換することです。検索やソートを予測可能にするため、または大文字小文字を無視したい場合にプログラマーはこれを行います。

## How to:
## 方法:
```rust
fn main() {
    let original = "Rust Programming!";
    let lowercase = original.to_lowercase();

    println!("Original: {}", original);
    println!("Lowercase: {}", lowercase);
}

// 出力
// Original: Rust Programming!
// Lowercase: rust programming!
```

## Deep Dive
## 詳細な解析
Rustには`to_lowercase`というメソッドがあり、Unicodeに基づいた正確な小文字変換を提供します。文字列を小文字にする操作はコンピュータの初期からありますが、Unicodeの登場で多言語に対応しようと多くの改良がなされました。代わりに`.to_ascii_lowercase()`を使うと英字のみを対象にした変換ができますが、全ての文字を正確に処理するには`.to_lowercase()`が最適です。内部的には、Rustの`.to_lowercase()`メソッドはUnicodeの正規化と特定の文字の変換ルールを利用して、一貫性と互換性を保ちながら動作します。

## See Also
## 関連する情報源
- Rustドキュメントの[String型のAPI](https://doc.rust-lang.org/std/string/struct.String.html)
- [Unicodeケースマッピング](https://www.unicode.org/reports/tr21/tr21-5.html)
- Rustの[ASCIIサポートについて](https://doc.rust-lang.org/std/ascii/)
