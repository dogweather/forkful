---
title:                "部分文字列の抽出"
date:                  2024-01-20T17:46:39.826681-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/extracting-substrings.md"
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
