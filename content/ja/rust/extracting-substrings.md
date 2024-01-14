---
title:    "Rust: 部分文字列の抽出"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ？

文字列から一部を抽出することは、プログラミングの世界では非常に一般的です。特に、テキスト処理やフォーマットの変換などのタスクを行う際には、文字列から特定のパターンや特定の位置の文字を取得する必要があります。そのため、Rustプログラミングでもよく使用される技術です。

## 作り方

```Rust
let s = String::from("Hello Rust");
let hello = &s[0..5];
println!("{}", hello); //Output: Hello
```

リストの最初の文字から5文字目までの部分文字列を抽出する方法を示しています。

## 深堀り

文字列から部分文字列を抽出する際、Rustではスライスと呼ばれる特殊なデータ型を使用します。スライスは、元の文字列データへの参照を保持するため、メモリを節約することができます。また、様々なメソッドを使用してスライスを操作することができるため、非常に柔軟に文字列の抽出が行えます。

## 参考リンク

- [Rust公式ドキュメント] (https://doc.rust-lang.org/book/ch04-03-slices.html)
- [Rustのスライスについて] (https://rust-jp.rs/tour-of-rust/data_types/slice.html)
- [文字列処理のパフォーマンスについて] (https://blog.fujimura1ji.net/2018/04/19/string-processing-in-rust/)