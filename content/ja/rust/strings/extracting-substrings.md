---
date: 2024-01-20 17:46:39.826681-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u4E00\u90E8\u3092\u53D6\u308A\u51FA\u3059\
  \u3053\u3068\u3092\u300C\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u62BD\u51FA\u300D\
  \u3068\u8A00\u3044\u307E\u3059\u3002\u30C7\u30FC\u30BF\u89E3\u6790\u3084\u5165\u529B\
  \u6574\u5F62\u306E\u969B\u306B\u3001\u7279\u5B9A\u306E\u6587\u5B57\u5217\u306E\u4E00\
  \u90E8\u5206\u3060\u3051\u304C\u5FC5\u8981\u306B\u306A\u308B\u304B\u3089\u3067\u3059\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:00.982751
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u4E00\u90E8\u3092\u53D6\u308A\u51FA\u3059\
  \u3053\u3068\u3092\u300C\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u62BD\u51FA\u300D\
  \u3068\u8A00\u3044\u307E\u3059\u3002\u30C7\u30FC\u30BF\u89E3\u6790\u3084\u5165\u529B\
  \u6574\u5F62\u306E\u969B\u306B\u3001\u7279\u5B9A\u306E\u6587\u5B57\u5217\u306E\u4E00\
  \u90E8\u5206\u3060\u3051\u304C\u5FC5\u8981\u306B\u306A\u308B\u304B\u3089\u3067\u3059\
  \u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から一部を取り出すことを「サブストリング抽出」と言います。データ解析や入力整形の際に、特定の文字列の一部分だけが必要になるからです。

## How to: (方法)
```Rust
fn main() {
    let text = "こんにちは、Rustの世界へようこそ！";
    let start = text.char_indices().nth(5).map(|(i, _)| i).unwrap_or(0);
    let end = text.char_indices().nth(10).map(|(i, _)| i).unwrap_or(text.len());
    
    let substring = &text[start..end]; // 5番目から9番目までの文字を抽出
    println!("{}", substring); // "、Rust"
}
```
出力:
```
、Rust
```

## Deep Dive (深掘り)
サブストリング抽出の前は、全文を扱うしかなかった。Rustでは`.char_indices()`, `.chars()`などのイテレータを利用してUnicode文字の正しい扱いができます。Rust以外では、異なるライブラリや関数を用いることもあります。Rustでは所有権と借用のシステムにより、サブストリングが元の文字列を参照していることを明確にできるので安全性が保たれます。

## See Also (関連情報)
- [Rust Book on Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust std::str API](https://doc.rust-lang.org/std/str/)
- [Rust by Example on Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
