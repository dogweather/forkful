---
date: 2024-01-26 04:45:43.132765-07:00
description: "\u4F7F\u3044\u65B9\uFF1A Rust\u306B\u306F\u8907\u7D20\u6570\u3092\u30B5\
  \u30DD\u30FC\u30C8\u3059\u308B\u7D44\u307F\u8FBC\u307F\u6A5F\u80FD\u306F\u3042\u308A\
  \u307E\u305B\u3093\u304C\u3001`num-complex`\u306E\u3088\u3046\u306A\u30AF\u30EC\u30FC\
  \u30C8\u304C\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u304F\u308C\u307E\u3059\u3002\u4F7F\
  \u3044\u65B9\u306F\u4EE5\u4E0B\u306E\u3068\u304A\u308A\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.811531-06:00'
model: gpt-4-0125-preview
summary: "Rust\u306B\u306F\u8907\u7D20\u6570\u3092\u30B5\u30DD\u30FC\u30C8\u3059\u308B\
  \u7D44\u307F\u8FBC\u307F\u6A5F\u80FD\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\
  `num-complex`\u306E\u3088\u3046\u306A\u30AF\u30EC\u30FC\u30C8\u304C\u30B5\u30DD\u30FC\
  \u30C8\u3057\u3066\u304F\u308C\u307E\u3059\u3002\u4F7F\u3044\u65B9\u306F\u4EE5\u4E0B\
  \u306E\u3068\u304A\u308A\u3067\u3059\uFF1A."
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

## 使い方：
Rustには複素数をサポートする組み込み機能はありませんが、`num-complex`のようなクレートがサポートしてくれます。使い方は以下のとおりです：

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let sum = a + b;
    let product = a * b;

    println!("Sum: {}", sum); // 合計: 3 - 1i
    println!("Product: {}", product); // 積: 14 - 5i
}
```
このマジックを実現するためには、`Cargo.toml`に`num_complex`を追加する必要があります。

## 深掘り
複素数は16世紀に考案されましたが、18世紀に入り、オイラーのような数学者がそれらを取り扱い始めたときに本当に隆盛を迎えました。

ネイティブの複素数操作をサポートしていない言語は、Rustのようにサードパーティーのライブラリに依存します。`num-complex`はそのようなクレートの一つであり、Rustに数値型とトレイトを提供することを目指す`num`クレートコレクションの一部です。

複素数を組み込みサポートしている言語（Pythonなど）や、標準ライブラリの一部として提供している言語（`<complex>`ヘッダーを持つC++など）がある一方で、Rustでは標準ライブラリを小さく保つという決定により、追加機能のためにコミュニティが作成したクレートに頻繁に頼ることになります。

## 参照
- [Rustの本](https://doc.rust-lang.org/book/): Rustと外部クレートの使用方法をさらに学ぶには。
- [複素数Wikipedia](https://en.wikipedia.org/wiki/Complex_number): 複素数についてより深く理解するために。
