---
title:                "文字列の長さを求める"
html_title:           "Rust: 文字列の長さを求める"
simple_title:         "文字列の長さを求める"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何？何のため？
文字列の長さを調べることは、プログラマーがよく行う処理です。これは、文字列に含まれる文字の数を確認するためです。

## 方法：
以下のコードブロックに、Rustで文字列の長さを求める方法の例と出力を示します。

```Rust
let string = String::from("Hello, world!");
let length = string.len();

println!("The length of the string is {}", length);
```

出力：
```
The length of the string is 13
```

## 深く掘り下げる：
文字列の長さを求めるという処理は、プログラミングにおいて重要な役割を果たしてきました。これは、文字列を扱う上で必要不可欠な機能であり、古くから使用されてきました。他にも、文字列の長さを求める方法として、ループを利用する方法や組み込みの関数を使用する方法などがあります。Rustでは、文字列の中に含まれるバイト数を計算し、その数を返すことで文字列の長さを求めます。

## 参考：
- [Rust公式ドキュメント](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [文字列の長さを求める方法についての記事 (英語)](https://www.thoughtco.com/string-length-function-958340)