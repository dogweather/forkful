---
title:                "数値の丸め処理"
date:                  2024-01-26T03:46:57.637881-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"

category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/rounding-numbers.md"
---

{{< edit_this_page >}}

## なにを、なぜ？
数値を丸めるとは、最も近い整数または特定の精度の分数に調整することを意味します。プログラマーが数値を丸めるのは、人間の可読性のために値を単純化するため、仕様要件を満たすため、または浮動小数点演算の計算オーバーヘッドを削減するためです。

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
