---
date: 2024-01-20 17:39:07.479286-07:00
description: "\u4F55\u3068\u306A\u305C? \u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\
  \u306B\u3059\u308B\u3068\u306F\u3001\u5168\u3066\u306E\u30A2\u30EB\u30D5\u30A1\u30D9\
  \u30C3\u30C8\u3092\u5C0F\u6587\u5B57\u7248\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\
  \u3067\u3059\u3002\u691C\u7D22\u3084\u30BD\u30FC\u30C8\u3092\u4E88\u6E2C\u53EF\u80FD\
  \u306B\u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\u5927\u6587\u5B57\u5C0F\u6587\
  \u5B57\u3092\u7121\u8996\u3057\u305F\u3044\u5834\u5408\u306B\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.708408-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u3059\u308B\u3068\u306F\
  \u3001\u5168\u3066\u306E\u30A2\u30EB\u30D5\u30A1\u30D9\u30C3\u30C8\u3092\u5C0F\u6587\
  \u5B57\u7248\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u691C\u7D22\
  \u3084\u30BD\u30FC\u30C8\u3092\u4E88\u6E2C\u53EF\u80FD\u306B\u3059\u308B\u305F\u3081\
  \u3001\u307E\u305F\u306F\u5927\u6587\u5B57\u5C0F\u6587\u5B57\u3092\u7121\u8996\u3057\
  \u305F\u3044\u5834\u5408\u306B\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

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
