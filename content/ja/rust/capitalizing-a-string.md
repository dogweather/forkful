---
title:                "Rust: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

あなたはRustプログラミングに興味を持っているかもしれませんが、今日はRustで文字列を大文字に変換する方法についてお話しします。文字列の大文字化は、文字列処理において非常に重要な役割を果たしています。それでは、どのようにして実現するのか見ていきましょう！

## 方法

まずは、文字列を大文字に変換するためにRustの標準ライブラリにあるメソッド```to_uppercase()```を使用します。このメソッドは、文字列全体を大文字に変換して新しい文字列を返します。

```
Rustでの文字列大文字化

fn main() {
    let s = "Hello, world!";
    println!("大文字に変換する前: {}", s);
    let upper = s.to_uppercase();
    println!("大文字に変換した後: {}", upper);
}
```

< output >

```
> 大文字に変換する前: Hello, world!
> 大文字に変換した後: HELLO, WORLD!
```

また、もし文字列の一部を大文字に変換したい場合は、```to_ascii_uppercase()```を使用することもできます。このメソッドでは、アルファベット以外の文字を除くすべての文字を大文字に変換します。

```
Rustでの文字列一部大文字化

fn main() {
    let s = "Hello, world!";
    println!("大文字に変換する前: {}", s);
    let upper = s[0..5].to_ascii_uppercase() + &s[5..];
    println!("一部を大文字に変換した後: {}", upper);
}
```

< output >

```
> 大文字に変換する前: Hello, world!
> 一部を大文字に変換した後: HELLO, world!
```

## 深く掘り下げる

では、```to_uppercase()```と```to_ascii_uppercase()```の違いについて少し詳しく見てみましょう。まず、```to_uppercase()```はUnicodeに基づいており、各言語における標準的な大文字小文字の変換規則に従います。一方、```to_ascii_uppercase()```はASCII文字に基づいています。

このような違いにより、例えばドイツ語の文字列であれば、```to_uppercase()```では```"ß"```を```"SS"```に変換しますが、```to_ascii_uppercase()```ではそのまま残ります。

## 参考

- [Rust標準ライブラリドキュメント](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)
- [Rust標準ライブラリドキュメント](https://doc.rust-lang.org/std/string/struct.String.html#method.to_ascii_uppercase)

## 用語集

- 大文字化：文字列をすべて大文字に変換すること。
- メソッド：オブジェクトやデータ型に対応する関数。
- アルファベット：文字の一種で、主に英語やフランス語などで使用される文字。