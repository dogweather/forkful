---
title:                "Rust: ランダムな数値の生成"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

Rustで偶数を生成する方法

## なぜ

乱数生成をする理由は多々あります。例えば、シミュレーションやゲームで使うことができます。また、ランダムなデータを生成することで、コンピューターの性能をテストすることもできます。

## 作り方

Rustで乱数を生成するには、`rand`クレートを使用します。まずは、`Cargo.toml`に`rand`を追加し、`main.rs`ファイルで`rand`をインポートします。

```Rust
[dependencies]
rand = "0.7.3"
```

```Rust
use rand::Rng;
```

次に、`main`関数の中で`rand`の`thread_rng`メソッドを使用し、乱数ジェネレーターを作成します。ここでは、1から10までの間の乱数を生成する例を示します。

```Rust
fn main() {
    let mut rng = rand::thread_rng();
    let random_number: u32 = rng.gen_range(1, 11);
    println!("ランダムな数: {}", random_number);
}
```

上記のコードを実行すると、毎回1から10までの間のランダムな数が出力されるでしょう。

## 深堀り

Rustの`rand`クレートには、様々なランダムなデータを生成するメソッドがあります。例えば、`gen`メソッドを使用すると、任意の型のランダムな値を生成することができます。また、`fill`メソッドを使用すると、与えられた変数や配列をランダムな値で埋めることができます。

さらに、`Distribution`トレイトを使用することで、特定の範囲や分布に沿ったランダムな値を生成することもできます。例えば、`Uniform`を使用すると、任意の範囲からランダムな数を生成することができます。

## 関連記事

- [`rand`クレートドキュメント](https://docs.rs/rand/0.7.3/rand/)
- [Rustの乱数生成について知る](https://codeburst.io/understanding-random-number-generation-in-rust-7717ebf940de)
- [乱数生成ライブラリで遊ぶ](https://guillaume-gomez.blog/?d=00/09/01)