---
title:                "「文字列を小文字に変換する」"
html_title:           "Rust: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ: 文字列を小文字に変換する必要性
Rust言語では、文字列を小文字に変換する必要がある場合があります。例えば、ユーザーから入力された文字列を正しく処理するためには、事前に小文字に変換しておく必要があります。また、文字列の比較や検索を行う際にも、大文字と小文字を区別することができないため、文字列を小文字に変換する必要があります。

## 方法: 文字列を小文字に変換するコード例
文字列を小文字に変換するには、Rust言語の標準ライブラリで使用できる`to_lowercase()`関数を使用します。以下のコード例をご覧ください。

```Rust
fn main() {
    let string = String::from("Hello World!");
    let lowercase_string = string.to_lowercase();
    println!("{}", lowercase_string);
}
```

このコードを実行すると、出力結果は`hello world!`となります。

## 深い掘り下げ: 文字列を小文字に変換する際の注意点
Rust言語では、文字列は不変な値として扱われます。つまり、文字列を小文字に変換しても元の文字列は変化しません。また、`to_lowercase()`関数はUnicodeに対応しており、日本語のようなマルチバイト文字にも正しく変換することができます。

## 参考リンク
- [Rust Documentation: to_lowercase()](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Rust Programming Language](https://www.rust-lang.org/)（公式サイト）
- [Rustのメリットとデメリットを分かりやすく解説！](https://eng-entrance.com/rust-moreless)（Rustの紹介記事）