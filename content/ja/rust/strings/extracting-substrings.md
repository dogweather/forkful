---
date: 2024-01-20 17:46:39.826681-07:00
description: "How to: (\u65B9\u6CD5) \u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u62BD\
  \u51FA\u306E\u524D\u306F\u3001\u5168\u6587\u3092\u6271\u3046\u3057\u304B\u306A\u304B\
  \u3063\u305F\u3002Rust\u3067\u306F`.char_indices()`,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.710350-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u62BD\u51FA\u306E\
  \u524D\u306F\u3001\u5168\u6587\u3092\u6271\u3046\u3057\u304B\u306A\u304B\u3063\u305F\
  \u3002Rust\u3067\u306F`.char_indices()`, `.chars()`\u306A\u3069\u306E\u30A4\u30C6\
  \u30EC\u30FC\u30BF\u3092\u5229\u7528\u3057\u3066Unicode\u6587\u5B57\u306E\u6B63\u3057\
  \u3044\u6271\u3044\u304C\u3067\u304D\u307E\u3059\u3002Rust\u4EE5\u5916\u3067\u306F\
  \u3001\u7570\u306A\u308B\u30E9\u30A4\u30D6\u30E9\u30EA\u3084\u95A2\u6570\u3092\u7528\
  \u3044\u308B\u3053\u3068\u3082\u3042\u308A\u307E\u3059\u3002Rust\u3067\u306F\u6240\
  \u6709\u6A29\u3068\u501F\u7528\u306E\u30B7\u30B9\u30C6\u30E0\u306B\u3088\u308A\u3001\
  \u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u304C\u5143\u306E\u6587\u5B57\u5217\u3092\
  \u53C2\u7167\u3057\u3066\u3044\u308B\u3053\u3068\u3092\u660E\u78BA\u306B\u3067\u304D\
  \u308B\u306E\u3067\u5B89\u5168\u6027\u304C\u4FDD\u305F\u308C\u307E\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

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
