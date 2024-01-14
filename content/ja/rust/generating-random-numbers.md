---
title:                "Rust: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

コンピュータプログラミングにおいて、ランダムな数値を生成することは非常に便利です。例えば、ゲームやシミュレーションなどのアプリケーションで、プレイヤーやコンピュータの行動をランダムに決める必要がある場合に使用されます。また、暗号学的なアプリケーションにおいてもランダムな数値の生成が重要です。

## 作り方

Rust言語において、ランダムな数値を生成する方法をご紹介します。まず、Rustの標準ライブラリに含まれているrandクレートをインポートします。次に、乱数生成器を作成し、その中にランダムな数値を生成するためのメソッドを呼び出します。

```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let random_number: u32 = rng.gen();
    println!("Random number: {}", random_number);
}
```

上記のコードでは、乱数生成器としてrand::thread_rng()を使用しています。そして、u32というデータ型を指定してランダムな数値を生成し、それを出力しています。もちろん、データ型や生成する数値の範囲を変更することも可能です。詳細な使い方はドキュメントを確認してください。

## 深堀り

実際には、ランダムな数値の生成は非常に複雑なことであり、暗号学的なアプリケーションにおいてはセキュリティ上の重要なポイントとなります。そのため、rust-cryptoというサードパーティーライブラリを使用することが推奨されています。

また、乱数生成器の種類やアルゴリズムについても、より詳しく学ぶことでより安全なランダムな数値の生成が可能になります。Rustのドキュメントや他の学習リソースを活用して、より深い知識を身につけることをお勧めします。

## 良い記事

- [RNGs and the Get There](https://www.rust-lang.org/ja/what-is-rust#:~:text=The%20Rust%20language%20has%20a%20strong%20focus%20on%20memory%20and%20concurrency%2C%20including%20safe%20data%20sharing%20between%20threads.) - Rustのドキュメントにおける乱数生成器に関する説明
- [The Rust Standard Library - Generating Random Numbers](https://doc.rust-lang.org/std/rand/index.html) - randクレートのドキュメント
- [Generating Random Numbers in Rust using the Standard Library and rand crate](https://dev.to/bbowers21/generating-random-numbers-in-rust-using-the-standard-library-and-rand-crate-1bmo) - randクレートを使用したランダムな数値の生成の方法について詳しく解説した記事

## 関連リンク

- [rust-crypto crate](https://github.com/DaGenix/rust-crypto) - 暗号学的なアプリケーションにおけるランダムな数値の生成に使用されるサードパーティーライブラリ
- [Rust By Example: Random](https://doc.rust-lang.org/stable/rust-by-example/std_misc/rand.html) - Rustの公式ドキュメントにおけるランダムな数値の生成のサンプルコード集
- [RNG Book](https