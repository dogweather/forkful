---
date: 2024-01-20 17:46:39.826681-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.803944-06:00'
model: gpt-4-1106-preview
summary: .
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
