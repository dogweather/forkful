---
title:    "Rust: ランダム数字の生成"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ乱数を生成するのか

乱数生成はコンピュータサイエンスやデータ分析など様々な分野で重要な役割を果たしています。ランダムな値を生成することで、現実世界の複雑な問題をシミュレーションすることができます。また、セキュリティや暗号化などの分野でも乱数生成は重要な要素となっています。

## 方法

Rustには標準ライブラリに乱数生成機能が組み込まれています。`rand`クレートを使用することで、簡単に乱数を生成することができます。

```Rust
// 必要な外部クレートをインポートする
use rand::{thread_rng, Rng};

// 0から10の間の乱数を生成する
let mut rng = thread_rng();
let rand_num = rng.gen_range(0, 10);
println!("Random number: {}", rand_num);
```

上記の例では、`thread_rng()`関数を使用してランダムなシード値を生成しています。そして、`gen_range()`関数を使用して0から10の間の乱数を生成しています。

また、`rand`クレートを使用することで、特定の確率分布に従う乱数を生成することもできます。例えば、正規分布に従う乱数を生成するには`StandardNormal`型を使用します。

```Rust
let mut rng = thread_rng();
let rand_num = rng.gen::<StandardNormal>();
println!("Random number: {}", rand_num);
```

## 深堀り

乱数生成は一見簡単そうに見えますが、実際には様々なアルゴリズムや数学的な考え方が組み込まれています。例えば、 `thread_rng`関数は安全な擬似乱数生成器に基づいており、`gen_range`関数のアルゴリズムはMersenne-Twisterアルゴリズムを使用しています。

乱数生成には偶然性や安全性など様々な要素が重要になってきます。そのため、Rustの標準ライブラリには様々なアルゴリズムや種類の乱数生成器が用意されています。その中から自分の目的に合った乱数生成器を選ぶことが重要です。

## 関連情報

- [The Rust Programming Language](https://doc.rust-lang.org/book/)
- [rand Crate Documentation](https://docs.rs/rand/0.6.5/rand/)
- [Wikipedia: Pseudorandom number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [Wikipedia: Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)

## 関連リンク

特にRust初心者の方におすすめの学習サイト:

- [Rust Crash Course](https://www.youtube.com/watch?v=zF34dRivLOw)
- [Rust by Example](https://doc.rust-lang.org/stable/rust-by-example/)