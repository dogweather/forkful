---
title:                "文字列を小文字に変換する"
html_title:           "Rust: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何で？：文字列を小文字に変換することは何ですか？プログラマーがそれをする理由は何ですか？

文字列を小文字に変換することは、文字列内のすべてのアルファベットを小文字に変換することを意味します。プログラマーは、文字列を正規化するために文字列を小文字に変換することがあります。これにより、同じ文字列であっても大文字と小文字の違いによるエラーを回避することができます。

## 方法：

```Rust
let my_string = "Hello, WORLD";
let lower_case = my_string.to_lowercase();

println!("{}", lower_case);
```
出力：hello, world

## 詳細：

1. 歴史的な文脈：文字列を小文字に変換することは、プログラミング言語によっては標準の機能として提供されている場合もあります。しかし、Rustでは標準のメソッドや関数は存在せず、文字列を小文字に変換するためにはto_lowercase()メソッドを使用する必要があります。

2. 代替手段：文字列を小文字に変換する方法として、パターンマッチングや正規表現を使用することもできます。しかし、to_lowercase()メソッドを使用する方が簡単であり、パフォーマンスも優れているため、推奨されます。

3. 実装の詳細：to_lowercase()メソッドは、内部でUnicodeの大文字と小文字に関する規則を使用して文字列を変換します。そのため、複雑な言語であっても適切に変換されることが保証されます。

## 関連情報：

- [Rust公式ドキュメント](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [文字列を小文字に変換する方法のベンチマークテスト](https://internals.rust-lang.org/t/psa-how-to-better-optimize-string-related-ops-like-comparison/8868/6)