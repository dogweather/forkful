---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？ (What & Why?)

乱数生成とは、予測できない一連の数値を生成することです。プログラマはゲーム、シミュレーション、テストデータ生成などのためにこれを利用します。

## どうやって： (How to:)

```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let n: u32 = rng.gen();
    println!("乱数: {}", n);
}
```
上記のコードを実行すると、例えば以下のような出力が得られます。

```Rust
乱数: 2938439
```

## 掘り下げる (Deep Dive)

乱数生成はコンピュータサイエンスの重要な部分で、1950年代から存在しています。Rustでは、`rand`クレートを利用して乱数を簡単に生成できます。しかし、便利なアルゴリズムの選択にはトレードオフがあります：疑似乱数生成器は十分なランダム性を提供する一方で、真の乱数生成器は予測不可能性を提供しますが、手間と資源を必要とします。また、`rand::thread_rng()`の実行は、OSの乱数生成器からシードを取得し、毎回異なる結果を出力します。

## 参照先 (See Also)

Rust の rand クレートに関する詳細な情報は[公式ドキュメンテーション](https://docs.rs/rand)をご覧ください。また、乱数生成の詳細については、[Wikipediaの乱数生成記事](https://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0%E7%94%9F%E6%88%90%E5%99%A8)も参照してみてください。