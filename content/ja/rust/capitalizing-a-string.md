---
title:                "文字列の先頭を大文字にする"
date:                  2024-01-19
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)

文字列を大文字化するとは、すべての文字を大文字に変換することです。読みやすさを向上させたり、データの一貫性を保つためにプログラマーはこれを行います。

## How to: (やり方)

Rustでは`.to_uppercase()`メソッドを使って文字列を大文字に変換できます。以下が例です。

```Rust
fn main() {
    let greeting = "hello, world!";
    println!("{}", greeting.to_uppercase());
}
```

実行結果:

```
HELLO, WORLD!
```

## Deep Dive (深掘り)

文字列を大文字化するのは新しいことではありませんが、Rust での方法は UTF-8 エンコーディングの完全なサポートとメモリ安全な操作を確保する点で独特です。ほかの言語では、`.toUpperCase()`や`.ToUpper()`などのメソッドが存在します。Rustの`.to_uppercase()`はイテレータを返すため、直接操作が可能ですが、その分、他の言語よりも少し手間がかかります。Rustでは、Unicodeのルールに従って効率的に文字列を大文字に変換するために多くの詳細が処理されています。

## See Also (関連情報)

- Rustの公式ドキュメント: [.to_uppercase()](https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase)
- Rust by Example: [文字列の操作](https://doc.rust-jp.rs/rust-by-example-ja/std/str.html)
- Unicode: [大文字と小文字のマッピング](https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt)
