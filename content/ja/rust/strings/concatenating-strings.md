---
title:                "文字列の連結"
aliases: - /ja/rust/concatenating-strings.md
date:                  2024-01-20T17:36:07.069474-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/concatenating-strings.md"
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
