---
title:    "Rust: 「パターンにマッチする文字を削除する」"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ

Rustプログラミングを学ぶと、より効率的で安全なコードを書くことができます。パターンに一致する文字を削除することは、コードをよりシンプルにするための有用な手段です。

## 使い方

```Rust
fn main() {
    let mut string = String::from("Hello, Rust!");
    let pattern = "Rust"; // 削除するパターン

    string.retain(|c| !pattern.contains(c)); // patternに一致しない文字を残す
    println!("{}", string); // Hello, !
}
```

## 深堀り

Rustでは`String`のメソッド`retain()`を使用して、指定した条件に合致しない文字を削除することができます。このメソッドはクロージャを受け取り、文字列内の各文字に対してこのクロージャを実行します。上記の例では、`pattern`に指定した文字列に一致しない文字を残すように`retain()`メソッドを使用しています。

## 参考リンク

- [The Rust Programming Language](https://www.rust-lang.org/ja)
- [Rustでプログラミングする際の注意点](https://honadmin.in/os/rust_tips.html)
- [Rust Advent Calendar 2019](https://qiita.com/advent-calendar/2019/rust)