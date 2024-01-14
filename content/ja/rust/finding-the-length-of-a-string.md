---
title:                "Rust: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列の長さを求めることの意義について。

文字列の長さを求めることは、文字列を操作する上で非常に重要な処理です。例えば、特定の文字列がある長さを超えるかどうかを確認したり、文字列の一部分を取り出したりする際には、その長さを知ることが必要になります。また、文字列の比較や検索を行う際にも、比較する文字列の長さを知ることが重要です。

## 方法
文字列の長さを求める方法について、Rustのコード例とサンプルの出力を交えて説明します。

```
Rust
fn main() {
    let s = String::from("こんにちは！");
    let length = s.len();
    println!("{}", length); // 6
}
```

上記のように、文字列の長さを求める場合は`String`型の`len()`メソッドを使用します。このメソッドは、文字列内のバイト数を返してくれます。日本語のようなマルチバイト文字を含む文字列の場合、バイト数と文字数が一致しないことに注意が必要です。

```
Rust
fn main() {
    let s = "Hello, world!";
    let length = s.chars().count();
    println!("{}", length); // 13
}
```

また、`chars()`メソッドと`count()`メソッドを組み合わせることで、テキストの実際の文字数を取得することもできます。

## ディープダイブ
さらに詳しく、文字列の長さを求める際に考慮すべきことについて紹介します。

- UTF-8エンコーディングには、バイトと文字の1対多の関係があるため、`len()`メソッドで返される値が実際の文字数と一致しないことがあります。
- `String`型の長さは可変であるため、文字列の追加や削除が行われると長さも変化します。しかし、バイト数は変化しないため注意が必要です。
- `count()`メソッドはヒープのエンコードされた文字を排除するため、実際の文字数を正確に求めることはできません。この場合、文字列内に含まれる特定の文字について、正確な文字数を求めたい場合は、正規表現を使用することができます。

## 参考リンク
- [The Rust Programming Language](https://doc.rust-jp.rs/book-ja/title-page.html)
- [String::len() documention](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [count() method documentation](https://doc.rust-lang.org/std/primitive.str.html#method.len)
- [Rust Regex crate](https://docs.rs/regex/1.5.4/regex/)