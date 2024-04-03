---
date: 2024-01-26 03:46:57.637881-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A Rust\u3067\u306F\u3001\u6570\u5024\
  \u306E\u4E38\u3081\u304C\u7C21\u5358\u3067\u3059\u3002`f32`\u3084`f64`\u578B\u306B\
  \u5BFE\u3057\u3066\u3001\u3053\u308C\u3089\u306E\u65B9\u6CD5\u3092\u8A66\u3057\u3066\
  \u307F\u3066\u304F\u3060\u3055\u3044\uFF1A."
lastmod: '2024-03-13T22:44:41.813969-06:00'
model: gpt-4-0125-preview
summary: "Rust\u3067\u306F\u3001\u6570\u5024\u306E\u4E38\u3081\u304C\u7C21\u5358\u3067\
  \u3059\u3002`f32`\u3084`f64`\u578B\u306B\u5BFE\u3057\u3066\u3001\u3053\u308C\u3089\
  \u306E\u65B9\u6CD5\u3092\u8A66\u3057\u3066\u307F\u3066\u304F\u3060\u3055\u3044\uFF1A\
  ."
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

## どのように：
Rustでは、数値の丸めが簡単です。`f32`や`f64`型に対して、これらの方法を試してみてください：

```rust
fn main() {
    let num = 2.34567;

    // 最も近い整数へ丸める
    let round = num.round();
    println!("Round: {}", round); // Round: 2

    // Floor - 数値以下の最大の整数
    let floor = num.floor();
    println!("Floor: {}", floor); // Floor: 2

    // Ceil - 数値以上の最小の整数
    let ceil = num.ceil();
    println!("Ceil: {}", ceil); // Ceil: 3

    // Truncate - 小数部分を持たない整数部分
    let trunc = num.trunc();
    println!("Truncate: {}", trunc); // Truncate: 2

    // 10のべき乗の近い倍数に丸める
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("2桁の小数点まで丸めた: {}", multiple_of_ten); // 2桁の小数点まで丸めた: 2.35
}
```

## 奥深く
歴史的に、丸めは無限小数や無理数を限られたデジタル空間に収めるのに不可欠であり、メモリが不足していた古代のコンピュータにとっては必須でした。そう、そろばんみたいですが、より計算重視です。

ネイティブのRustメソッドに代わるものとしては、次のようなものがあります：
1. デフォルトで丸める文字列フォーマッティングの`format!`マクロ。
2. よりきめ細かい制御を提供する外部クレート、例えば`round`クレート。

内部では、Rustの丸め操作はIEEE規格に準拠しています—つまり、数学の先生が望むように丸められますという技術用語です。さらに、二進表現のため、0.1のような一部の数値は、二進数での無限の表現のため、伝統的な方法で丸めることができません。

## 参照
- Rustのプリミティブ型メソッドについてのドキュメント：https://doc.rust-lang.org/std/primitive.f64.html
- 浮動小数点演算のIEEE標準 (IEEE 754)：https://ieeexplore.ieee.org/document/4610935
- より複雑な丸めのための"round"クレート：https://crates.io/crates/round
