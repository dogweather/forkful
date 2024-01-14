---
title:                "Rust: 文字列の長さを見つける"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列の長さを求めることについて、なぜ誰かが関わる必要があるのかを見てみましょう。

## 方法
文字列の長さを求める方法は、Rustプログラミング言語を使用することで非常に簡単に行うことができます。

```Rust
// 文字列を定義する
let str = "こんにちは！";
// 文字列の長さを求める
let length = str.len();
// 結果を出力する
println!("文字列の長さは{}です", length);

// 出力結果：
// 文字列の長さは6です
```

## ディープダイブ
文字列の長さを求めるには、Rustの標準ライブラリに含まれている`len()`メソッドを使用します。このメソッドは、文字列のバイト数を数えて返します。Unicodeの場合は、文字の数を数えるために、`chars()`メソッドを使用する必要があります。

しかし、Rustでは文字列スライスの長さを求めることも可能です。文字列スライスは、文字列から一部分を切り取ったもので、`[start..end]`のように指定します。文字列スライスの場合、`len()`メソッドは文字の数を数えます。

## 参考
- Rust公式ドキュメント: https://doc.rust-lang.org/stable/std/primitive.str.html#method.len
- Rust by Example - Strings: https://doc.rust-lang.org/rust-by-example/std/str.html