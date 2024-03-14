---
date: 2024-01-20 17:43:13.457383-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\
  \u306B\u30DE\u30C3\u30C1\u3059\u308B\u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u3068\
  \u306F\u3001\u6587\u5B57\u5217\u3092\u6E05\u6D44\u5316\u3057\u3066\u3001\u4E0D\u8981\
  \u306A\u90E8\u5206\u3092\u53D6\u308A\u9664\u304F\u3053\u3068\u3067\u3059\u3002\u3053\
  \u308C\u306F\u3001\u30C7\u30FC\u30BF\u306E\u6574\u5F62\u3001\u7121\u52B9\u306A\u5165\
  \u529B\u306E\u30D5\u30A3\u30EB\u30BF\u30EA\u30F3\u30B0\u3001\u307E\u305F\u306F\u4E00\
  \u8CAB\u6027\u3092\u4FDD\u3064\u305F\u3081\u306B\u884C\u308F\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.796242-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\
  \u306B\u30DE\u30C3\u30C1\u3059\u308B\u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u3068\
  \u306F\u3001\u6587\u5B57\u5217\u3092\u6E05\u6D44\u5316\u3057\u3066\u3001\u4E0D\u8981\
  \u306A\u90E8\u5206\u3092\u53D6\u308A\u9664\u304F\u3053\u3068\u3067\u3059\u3002\u3053\
  \u308C\u306F\u3001\u30C7\u30FC\u30BF\u306E\u6574\u5F62\u3001\u7121\u52B9\u306A\u5165\
  \u529B\u306E\u30D5\u30A3\u30EB\u30BF\u30EA\u30F3\u30B0\u3001\u307E\u305F\u306F\u4E00\
  \u8CAB\u6027\u3092\u4FDD\u3064\u305F\u3081\u306B\u884C\u308F\u308C\u307E\u3059\u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から特定のパターンにマッチする文字を削除するとは、文字列を清浄化して、不要な部分を取り除くことです。これは、データの整形、無効な入力のフィルタリング、または一貫性を保つために行われます。

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
