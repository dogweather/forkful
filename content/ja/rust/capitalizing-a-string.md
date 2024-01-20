---
title:                "文字列を大文字にする"
html_title:           "Rust: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Rustによる文字列の最初の文字を大文字に変換する（キャピタライズする）

## 何となぜ？

文字列キャピタライズとは、文字列の最初の文字を大文字に変換することを指します。プログラマーはこれを行うことで、タイトルや文章の初めなどでエイリアスを正しく表示するために使用します。

##やり方:

Rustでは、文字列の最初の文字を大文字にするには、`to_uppercase()`と`chars()`を使用します。以下に例を示します。

```Rust
fn main() {
    let s = "hello world";
    let capitalized = s.chars().enumerate()
        .map(|(i,c)| if i == 0 { c.to_uppercase().collect::<String>() } else { c.to_string() })
        .collect::<String>();
    println!("{}", capitalized);
}
```
上記のコードを実行すると、出力として"Hello world"を得ることができます。

## 深層

文字列のキャピタライズは、プログラミングの歴史の初期段階から存在する機能の一つです。この操作を行ったり、他の文字列操作を行ったりするためのさまざまな方法が歴史的に存在します。

Rustでは他の方法も試すことができます。例えば、`match`文を使用する方法です。

```Rust
fn main() {
    let s = "hello world";
    let capitalized = match s.chars().next() {
        None => String::new(),
        Some(first_char) => first_char.to_uppercase().collect::<String>() + &s[1..],
    };
    println!("{}", capitalized);
}
```

上記のコードも同様に"Hello world"を出力します。

ただし、Rustでは文字列のインデクシングが一筋縄ではいかないため、この方法には注意が必要です。

## さらに詳しく

- [Rustの文字列ドキュメンテーション](https://doc.rust-lang.org/std/string/index.html)
- [Rustのcharメソッドドキュメンテーション](https://doc.rust-lang.org/std/primitive.char.html)