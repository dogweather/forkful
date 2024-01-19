---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の長さを数えるとは、文字列がいくつの「文字」から構成されているかを算出することです。これは、プログラマが文字列の容量を計算したり、特定の操作の効率を高めたりするために行います。

## 使い方:

以下に文字列の長さを取得するRustのコード例を示します：

```Rust
fn main() {
    let s = "こんにちは、世界!";
    println!("Length: {}", s.chars().count());
}
```

このコードを実行すると、出力結果は次のとおりです：

```Rust
Length: 8
```

## 詳細:

文字列の長さを計算することはプログラミングの歴史の初期から存在しています。しかし、異なる言語とエンコードは異なる方法でこの問題を取り扱います。たとえば、C言語では、ヌルターミネータを検出するまで文字の配列をスキャンして長さを見つけます。

Rustでは、`str::len()`関数でバイト長を返す代わりに、`chars().count()`を使用してUnicode文字数を数えます。これは、文字列が任意のUTF-8エンコーディングを含む可能性があるためです。

## 参考資料：

文字列長とその計算についてさらに学びたい方は、以下のリンクをご参照ください：

- Rust By Example の [Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- The Rust Programming Language の [Ch. 8.2 – Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)