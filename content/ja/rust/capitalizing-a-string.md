---
title:                "Rust: 「文字列を大文字化する」"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
Rustで文字列をキャピタライズする理由を説明します。文字列を大文字に変換する必要性は、プログラムで正しい出力を得るために不可欠です。

## 使い方
文字列をキャピタライズする方法はいくつかあります。まず、標準ライブラリの`to_uppercase()`メソッドを使用する方法があります。例えば、以下のコードで文字列を大文字に変換することができます。

```Rust
let my_string = "hello";
let capitalized_string = my_string.to_uppercase();
println!("{}", capitalized_string);
```

出力は`HELLO`となります。

また、`String`型には`to_uppercase()`メソッドが存在しないため、`Vec<char>`型に変換した後、`to_ascii_uppercase()`メソッドを使用することもできます。例えば、以下のコードで同じ結果が得られます。

```Rust
let my_string = "hello".to_string();
let char_vector: Vec<char> = my_string.chars().collect();
let capitalized_char_vector = char_vector.to_ascii_uppercase();
let capitalized_string: String = capitalized_char_vector.iter().collect();
println!("{}", capitalized_string);
```

出力は同じく`HELLO`となります。

## 詳細
簡単な方法以外にも、パフォーマンスを最適化したカスタム実装方法があります。例えば、`chars()`メソッドを使用して文字列をイテレータに変換し、各文字を`to_ascii_uppercase()`メソッドで大文字に変換し、新しい文字列に追加する方法です。この方法は、文字列が大きくなるにつれて効率的になります。

さらに、マルチスレッド処理を用いた並列処理も可能です。複数のスレッドで文字列を分割し、同時に処理することで、パフォーマンスをさらに向上させることができます。

## はじめてのRust
うまくいかない場合に備えてカスタム実装と並列処理を組み合わせた完全なコード例を用意しました。是非参考にしてください。

```Rust
use std::thread;
use regex::Regex;

fn capitalize_string(string: String) -> String {
    let char_vector: Vec<char> = string.chars().collect();
    let mut capitalized_char_vector: Vec<char> = Vec::new();

    let joined_string: String = char_vector
        .par_chunks(4)
        .map(|chunk| {
            let mut chunk_vec: Vec<char> = Vec::new();
            for c in chunk {
                chunk_vec.push(c.to_ascii_uppercase());
            }
            chunk_vec
        })
        .flat_map(|x| x)
        .collect();

    capitalized_char_vector.extend(joined_string.chars());

    let capitalized_string: String = capitalized_char_vector.iter().collect();
    capitalized_string
}

fn main() {
    let my_string = String::from("hello");
    let capitalized_str = capitalize_string(my_string);
    println!("{}", capitalized_str);
}

// Output: HELLO
```

## 参考リンク
- [The Rust Programming Language](https://www.rust-lang.org/)
- [The Rust Standard Library](https://doc.rust-lang.org/std/)
- [Rustの並列処理パッケージ「Rayon」を使ってみる](https://postd.cc/trying-out-the-rayon-parallel-programming-ecosystem-in-rust/)
- [Rustで30秒以内に文字列を大文字に変換する方法](https://blog.mitsutaka.org/2019/09/rust%E3%81%A730%E7%A7%92%E4%BB%A5%E5%86%85%E3%81%AB%E6%96%87%E5%AD%97%E5%