---
title:                "複素数の扱い方"
aliases:
- /ja/rust/working-with-complex-numbers/
date:                  2024-01-26T04:45:43.132765-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
複素数には実部と虚部があり、エンジニアリング、物理学、コンピュータグラフィックスなど様々な分野で不可欠です。プログラマーは、通常の実数では扱えない方程式を解くためにそれらを使用します。

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
