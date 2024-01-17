---
title:                "ランダムな数値を生成する"
html_title:           "Rust: ランダムな数値を生成する"
simple_title:         "ランダムな数値を生成する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

# ランダムな数値を生成するには？

## 何で、何故？
ランダムな数値を生成することは、コンピュータープログラマーにとって非常に重要なタスクの一つです。ランダムな数値とは、コンピューターが自動的に生成し、予測不能な値のことを指します。プログラマーがランダムな数値を利用する理由は様々ですが、主な用途はゲームやセキュリティーを担保するためです。

## 方法：
Rustでは、ランダムな数値を生成するために```rand```ライブラリーを使用します。まず、プロジェクトの```Cargo.toml```ファイルに次の行を追加します。

```
rand = "0.8.3"
```

次に、生成したいランダムな数値の範囲を指定し、乱数ジェネレーターを作成します。

```Rust
use rand::Rng;
let random_number = rand::thread_rng().gen_range(1, 11);
```

上記の例では、1から10までのランダムな数値を生成しています。

## 詳細説明：
ランダムな数値を生成する方法には様々なアプローチがありますが、Rustでは標準の乱数ジェネレーターである```rand::thread_rng```を使用することが推奨されています。また、より高度な乱数ジェネレーターを使用する場合は、外部のライブラリーを導入することもできます。

## 関連リンク：
- [Rustの公式ドキュメンテーション](https://doc.rust-lang.org/std/rand/)
- [外部の乱数ジェネレーターであるrand](https://crates.io/crates/rand)
- [ランダムな数値を生成する方法の比較](https://stackoverflow.com/questions/54075171/which-random-number-package-should-i-use-in-rust)