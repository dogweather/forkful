---
title:                "Rust: 文字列を小文字に変換する"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

Rustで文字列を小文字に変換する理由は、文字列の大文字と小文字を区別せずに検索や比較を行うためです。

## 方法

まず、`std::string::String`型の文字列を取得します。次に、`to_lowercase()`関数を使用して文字列を小文字に変換します。

```Rust
let my_string = String::from("Hello World!");
let lowercase_string = my_string.to_lowercase();
println!("{}", lowercase_string);
```

このコードの出力は、`hello world!`となります。

## 深堀り

Rustでは、文字列をUTF-8形式で扱います。そのため、`to_lowercase()`関数はUTF-8文字列を適切に処理することができます。

また、この関数はUnicodeの規則に基づいて文字列を小文字に変換するため、例えば`İ`といったラテン文字の大文字は、`i`としてではなく、`i`と`̇`(ドット付きの小文字i)として変換されます。

## 併せて読みたい

- [Rust公式ドキュメント](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Unicodeの様々な文字変換の仕組みについて詳しく知る](https://unicode.org/reports/tr21/)