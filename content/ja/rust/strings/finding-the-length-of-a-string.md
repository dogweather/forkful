---
date: 2024-01-20 17:48:04.583222-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.163917-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

## How to: (方法)
```Rust
fn main() {
    let greeting = "こんにちは";
    let length = greeting.chars().count(); // 文字の数を数える
    println!("Length: {}", length); // 長さを表示
}
```
出力例:
```
Length: 5
```

## Deep Dive (深掘り)


### Historical Context (歴史的背景)
Rustは性能と安全性に重点を置いて設計されている。文字列の長さを取得する方法も同様に、安全に効率的に行うことができる。

### Alternatives (代替手段)
`.chars().count()`以外にも、`.len()`メソッドを使いバイト単位で長さを得ることができる。ただし、`.len()`はUTF-8の文字列を正確に数えるためには適していない場合がある。

### Implementation Details (実装の詳細)
UTF-8文字列では、全ての文字が1バイトとは限らないため、`.chars().count()`はより信頼性が高い。これはイテレータを使い文字列をUnicodeスカラー値に分解し、正確な文字数をカウントする。

## See Also (関連情報)
- [Rust Book: Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust Documentation: std::str](https://doc.rust-lang.org/std/str/)
- [Rust by Example: Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
