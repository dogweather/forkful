---
title:                "ランダム数の生成"
date:                  2024-01-20T17:49:53.875274-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
乱数生成とは、予測できない数字を作ることです。ゲーム、セキュリティ、シミュレーションでよく使います。ランダム性は予想外の振る舞いを実現し、リアルさや公平さを提供します。

## How to (どうやって？)
Rustの標準ライブラリは乱数生成を直接サポートしていません。`rand`クレートを使いましょう。Cargo.tomlに依存関係を追加して、簡単な例を試してみてください。

Cargo.toml:
```toml
[dependencies]
rand = "0.8.5"
```

main.rs:
```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    
    let rand_num: u32 = rng.gen(); // タイプに合わせた乱数
    println!("生成された乱数: {}", rand_num);

    let rand_range = rng.gen_range(0..10); // 0から9までの範囲
    println!("範囲内で生成された乱数: {}", rand_range);

    let rand_bool: bool = rng.gen(); // ランダムな真偽値
    println!("生成された真偽値: {}", rand_bool);
}
```

実行結果のサンプル:
```
生成された乱数: 3837524226
範囲内で生成された乱数: 5
生成された真偽値: true
```

## Deep Dive (深掘り)
乱数生成はコンピューターにとって難しい。真の乱数は物理的現象に基づくが、多くは擬似乱数生成器(PRG)を使用する。PRGは初期値(シード)から開始し、複雑なアルゴリズムを通じて数列を生成する。

Rustの`rand`クレートはPRGに`ThreadRng`や`StdRng`を使う。これらはそれぞれ異なる速度と安全性のトレードオフを持っている。`rand::thread_rng()`は一般的には十分な安全性と速度を提供します。

乱数生成の歴史は長く、コンピュータの初期から重要な役割を果たしてきました。高品質のPRGは暗号学やデータ分析など、多岐に渡る分野で使われています。

## See Also (関連情報)
- ランダム性についての詳細: [https://en.wikipedia.org/wiki/Randomness](https://en.wikipedia.org/wiki/Randomness)
