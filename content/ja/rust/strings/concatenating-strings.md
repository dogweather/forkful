---
date: 2024-01-20 17:36:07.069474-07:00
description: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\u8907\u6570\u306E\
  \u6587\u5B57\u5217\u3092\u304F\u3063\u3064\u3051\u3066\u4E00\u3064\u306B\u3059\u308B\
  \u3053\u3068\u3067\u3059\u3002\u30C7\u30FC\u30BF\u3092\u307E\u3068\u3081\u305F\u308A\
  \u3001\u30E6\u30FC\u30B6\u30FC\u306B\u898B\u3084\u3059\u3044\u5F62\u3067\u60C5\u5831\
  \u3092\u8868\u793A\u3059\u308B\u305F\u3081\u306B\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u6587\u5B57\u5217\u3092\u9023\u7D50\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:00.986809
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\u8907\u6570\u306E\
  \u6587\u5B57\u5217\u3092\u304F\u3063\u3064\u3051\u3066\u4E00\u3064\u306B\u3059\u308B\
  \u3053\u3068\u3067\u3059\u3002\u30C7\u30FC\u30BF\u3092\u307E\u3068\u3081\u305F\u308A\
  \u3001\u30E6\u30FC\u30B6\u30FC\u306B\u898B\u3084\u3059\u3044\u5F62\u3067\u60C5\u5831\
  \u3092\u8868\u793A\u3059\u308B\u305F\u3081\u306B\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u6587\u5B57\u5217\u3092\u9023\u7D50\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の連結とは、複数の文字列をくっつけて一つにすることです。データをまとめたり、ユーザーに見やすい形で情報を表示するためにプログラマーは文字列を連結します。

## How to: (やり方)
Rustでは文字列を連結する方法がいくつかあります。シンプルな例を見てみましょう。

```Rust
fn main() {
    let greeting = "こんにちは".to_string();
    let name = "世界";
    let exclamation = "!";

    // 方法1: `+`演算子を使う
    let message = greeting + ", " + name + exclamation;
    println!("{}", message); // 出力: こんにちは, 世界!

    // 方法2: format! マクロを使う
    let formatted_message = format!("{}, {}{}", greeting, name, exclamation);
    println!("{}", formatted_message); // 出力: こんにちは, 世界!
}
```

## Deep Dive (さらなる情報)
Rustではメモリ安全性を重視していて、文字列連結も例外ではありません。`+`演算子を使う際、所有権の観点から最初の変数が消費されます。つまり、`greeting + ", "`とすると`greeting`はもう使えません。これを避けるために`format!`マクロが使われることが多いです。

過去には他の言語のような`.concat()`メソッドや`.join()`メソッドがRustにもありましたが、現在は`format!`マクロや`+`演算子が推奨されています。これらの方法は所有権のルールや型チェックに沿って安全に文字列を結合できます。

## See Also (関連リンク)
- [Rust Documentation - Strings](https://doc.rust-lang.org/stable/book/ch08-02-strings.html)
- [Rust by Example - Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [The Rust Programming Language - Understanding Ownership](https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html)
