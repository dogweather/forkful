---
date: 2024-01-20 17:43:13.457383-07:00
description: "How to: (\u3084\u308A\u65B9:) \u3053\u306E\u30B3\u30FC\u30C9\u3067\u306F\
  `.retain()`\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3057\u3001\u6570\u5024\u306B\
  \u30DE\u30C3\u30C1\u3059\u308B\u6587\u5B57\u3092\u524A\u9664\u3057\u3066\u3044\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.705209-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9:) \u3053\u306E\u30B3\u30FC\u30C9\u3067\u306F`.retain()`\u30E1\
  \u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3057\u3001\u6570\u5024\u306B\u30DE\u30C3\u30C1\
  \u3059\u308B\u6587\u5B57\u3092\u524A\u9664\u3057\u3066\u3044\u307E\u3059\u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

## How to: (やり方:)
```Rust
fn main() {
    let mut greeting = String::from("こんにちわ、世界! 123");
    println!("Before: {}", greeting);

    greeting.retain(|c| !c.is_numeric());
    println!("After: {}", greeting);
}

// 出力:
// Before: こんにちわ、世界! 123
// After: こんにちわ、世界! 
```
このコードでは`.retain()`メソッドを使用し、数値にマッチする文字を削除しています。

## Deep Dive (深掘り)
Rustでは`String`の`.retain()`メソッドが便利です。このメソッドはベクタにも存在し、条件に合致しない値を効率良く削除できます。過去には正規表現などの方法も使用されましたが、Rustでは`regex`クレートを使用することもできます。`.retain()`は元の文字列を変更する点で`filter()`イテレータと異なります。内部的には、適合しない文字をシフトさせながら効率的に操作を行います。

## See Also (参照)
- Rustの公式ドキュメント: [std::string::String](https://doc.rust-lang.org/std/string/struct.String.html)
- `regex`クレートのドキュメント: [regex](https://docs.rs/regex/)
- Rustプログラミング言語の書籍: [The Rust Programming Language](https://doc.rust-lang.org/book/)
