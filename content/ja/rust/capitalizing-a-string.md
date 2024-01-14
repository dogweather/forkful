---
title:    "Rust: 文字列の大文字化"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

今日は、Rustプログラミングの魅力的な機能の一つである文字列の大文字化について紹介します。

## Why
文字列を大文字化する理由は様々です。例えば、ユーザーによって入力されるデータを統一的に整形する必要がある場合や、大文字小文字を区別しない検索を行う必要がある場合などが挙げられます。Rustの大文字化機能を使用することで、これらの問題を解決することができます。

## How To
文字列を大文字化するには、`to_uppercase()`メソッドを使用します。下の例では、`name`という変数に代入された文字列を大文字化して出力しています。

```Rust
let name = "John";
println!("{}", name.to_uppercase());
```

出力結果：
```
JOHN
```

## Deep Dive
Rustでは、標準ライブラリの`String`型に対して`to_uppercase()`メソッドが実装されています。このメソッドは、UTF-8エンコーディングの文字列に対応しており、英語以外の言語でも正しく大文字化することができます。また、ASCII以外の文字（特に非西洋言語の文字）も正しく変換することができます。

## See Also
- [Rust Documentation](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)
- [Unicode Standard](https://unicode.org/faq/casemap_charprop.html#caseprop)

以上で、Rustの文字列の大文字化についての紹介を終わります。この機能を活用して、より柔軟な文字列操作を実現しましょう。