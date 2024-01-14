---
title:    "C++: 乱数の生成"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ乱数を生成する必要があるのか

プログラマーにとって、乱数を生成することは何かを模倣するだけでなく、ランダム性を与えることができる便利な方法です。例えば、ゲームを作る時に敵の行動を制御したり、データのサンプリングを行ったりする場合に有用です。

## 乱数を生成する方法

C++で乱数を生成する方法を見ていきましょう。以下のようなコードを使用して、`rand()`関数を呼び出すことで乱数を生成することができます。

```C++
#include <iostream>
#include <cstdlib>
#include <ctime>

int main() {
    // 現在の時刻を使用してシード値を設定する
    srand(time(0));

    // 0から10までの範囲で乱数を生成する
    int randomNumber = rand() % 11;

    // 生成された乱数を出力する
    std::cout << "Random Number: " << randomNumber << std::endl;

    return 0;
}
```

実行結果は以下のようになります。

```
Random Number: 7
```

## 乱数生成の深層

乱数を生成する方法は多いですが、そのアルゴリズムの一つに「線形合同法（Linear Congruential Generator）」があります。この方法では、適切なシード値を設定することで準備された数値シーケンスを生成することができます。

また、乱数の分布にも重要な影響を与えることができます。例えば、一様分布を生成するためには、生成された数を最大値で割ることで実現できます。簡単ですが、このような細かな調整が乱数生成の精度を向上させることができます。

## 関連リンク

* [Generating Random Numbers (C++)](https://www.learncpp.com/cpp-tutorial/generating-random-numbers/)
* [C++ Reference - rand()](https://www.cplusplus.com/reference/cstdlib/rand/)
* [C++ Reference - srand()](https://www.cplusplus.com/reference/cstdlib/srand/)
* [Linear Congruential Generator](https://en.wikipedia.org/wiki/Linear_congruential_generator)