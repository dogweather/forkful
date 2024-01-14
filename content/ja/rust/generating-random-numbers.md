---
title:    "Rust: ランダムな数値の生成"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# なぜ乱数を生成するのか

乱数を生成することは、ゲームや暗号化、さまざまな実験などさまざまな目的で使用されます。Rustでは、randクレートを使用して簡単に乱数を生成することができます。ここでは、乱数を生成する方法を紹介します。

## 方法

乱数を生成するためには、まず` cargo.toml `ファイルに` rand`クレートを追加する必要があります。

```
[dependencies]
rand = "0.8.1"
```

次に、乱数を使用するファイルで`use rand`を追加し、`Rng`トレイトを実装するために`rand::Rng`を使用します。

```
use rand::Rng;
```

最後に、以下のように`gen_range`メソッドを使用して、指定した範囲内の乱数を生成することができます。

```
let random_number = rand::thread_rng().gen_range(1, 101);
```

この場合、1から100までの範囲内の乱数が生成されます。詳細なコーディング例を以下に示します。

```
use rand::Rng;

fn main() {
    let random_number = rand::thread_rng().gen_range(1, 101);
    println!("The generated random number is: {}", random_number);
}
```

上記のコードを実行すると、1から100までの範囲内の乱数が出力されます。実行結果の例を以下に示します。

```
The generated random number is: 44
```

## 深堀り

ランダムな乱数を生成するために、Rustではメルセンヌツイスターを使用しています。これは、周期性が非常に長い高速な乱数生成アルゴリズムです。また、`thread_rng()`メソッドを使用することで、スレッド固有の乱数生成器を生成し、相互に排他的なシードを使用して乱数を生成することができます。

## 他の記事を読む

- [Rustでの乱数生成 | The Book 日本語版](https://doc.rust-jp.rs/book/second-edition/ch08-02-strings.html)
- [Rustの乱数生成 | Qiita](https://qiita.com/nagisa-ito/items/7e5017ee453e3bad2bb2)

# 参考

- [randクレートのドキュメント](https://docs.rs/rand/0.8.1/rand/)
- [Rustで乱数を生成する方法 | Rustパターン](https://rust-pattern.com/rand/)