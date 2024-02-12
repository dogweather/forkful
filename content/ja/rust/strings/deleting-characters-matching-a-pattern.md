---
title:                "パターンに一致する文字を削除する"
aliases:
- /ja/rust/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:13.457383-07:00
model:                 gpt-4-1106-preview
simple_title:         "パターンに一致する文字を削除する"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/deleting-characters-matching-a-pattern.md"
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
