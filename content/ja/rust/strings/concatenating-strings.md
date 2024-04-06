---
date: 2024-01-20 17:36:07.069474-07:00
description: "How to: (\u3084\u308A\u65B9) Rust\u3067\u306F\u6587\u5B57\u5217\u3092\
  \u9023\u7D50\u3059\u308B\u65B9\u6CD5\u304C\u3044\u304F\u3064\u304B\u3042\u308A\u307E\
  \u3059\u3002\u30B7\u30F3\u30D7\u30EB\u306A\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\
  \u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.713512-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Rust\u3067\u306F\u6587\u5B57\u5217\u3092\u9023\u7D50\
  \u3059\u308B\u65B9\u6CD5\u304C\u3044\u304F\u3064\u304B\u3042\u308A\u307E\u3059\u3002\
  \u30B7\u30F3\u30D7\u30EB\u306A\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\
  \u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

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
