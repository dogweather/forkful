---
date: 2024-01-20 17:48:04.583222-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u77E5\u308B\u3053\u3068\u306F\
  \u3001\u305D\u306E\u6587\u5B57\u5217\u304C\u4F55\u6587\u5B57\u304B\u3089\u306A\u308B\
  \u304B\u3092\u6570\u3048\u308B\u3053\u3068\u3060\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u30C7\u30FC\u30BF\u306E\u51E6\u7406\u3084\u3001\u5165\u529B\u306E\u691C\
  \u8A3C\u3092\u884C\u3046\u305F\u3081\u306B\u3053\u306E\u60C5\u5831\u3092\u4F7F\u3046\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:00.985612
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u77E5\u308B\u3053\u3068\u306F\
  \u3001\u305D\u306E\u6587\u5B57\u5217\u304C\u4F55\u6587\u5B57\u304B\u3089\u306A\u308B\
  \u304B\u3092\u6570\u3048\u308B\u3053\u3068\u3060\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u30C7\u30FC\u30BF\u306E\u51E6\u7406\u3084\u3001\u5165\u529B\u306E\u691C\
  \u8A3C\u3092\u884C\u3046\u305F\u3081\u306B\u3053\u306E\u60C5\u5831\u3092\u4F7F\u3046\
  \u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の長さを知ることは、その文字列が何文字からなるかを数えることだ。プログラマーはデータの処理や、入力の検証を行うためにこの情報を使う。

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
