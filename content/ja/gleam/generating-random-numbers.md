---
title:    "Gleam: ランダムな数字を生成する"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# なぜ__乱数の生成に取り組むのか？

乱数は、ゲームやシミュレーション、そしてセキュリティー分野で重要な役割を果たします。Gleamプログラミング言語で乱数を生成する方法を学ぶことで、これらの分野でより高度なプログラムを作成することができます。

## 方法

```Gleam
import gleam/random

fn main() {
  // 0以上10未満の整数を生成する例
  let random_number = random.int(0, 10)
  
  // 0以上1未満の浮動小数点数を生成する例
  let random_float = random.float(0.0, 1.0)
  
  // リストからランダムな要素を選択する例
  let list = [1, 2, 3, 4, 5]
  let random_choice = random.choice(list)
  
  // 乱数生成を初期化する例
  let random_generator = random.init(123456789)
  
  // 乱数生成器を使用して0以上100未満の整数を生成する例
  let random_number = random_generator.int(0, 100)
}
```

```Gleam
// 出力例
random_number = 7
random_float = 0.527
random_choice = 3
random_number = 42
```

## 深堀り

乱数の生成は、擬似乱数と真の乱数の2種類に分類されます。Gleamでは、擬似乱数を生成するためにメルセンヌ・ツイスター(Mersenne Twister)アルゴリズムが使用されています。ただし、このアルゴリズムは事前に設定したシード値に基づいて乱数を生成するため、実行ごとに同じシード値を使用すると同じ乱数が生成されてしまいます。そのため、乱数生成時にseedを変えることで、よりランダムな値を生成することができます。

## 併せて読みたい

- [Gleamの公式ドキュメント：乱数](https://gleam.run/reference/stdlib.html#random)
- [ブログ記事：Gleamで乱数を生成する方法](https://blog.example.com/gleam-random)
- [GleamのGitHubリポジトリ](https://github.com/your_username/gleam)