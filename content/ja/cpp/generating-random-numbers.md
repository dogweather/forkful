---
title:                "C++: ランダムな数字を生成する"
simple_title:         "ランダムな数字を生成する"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# なぜランダムな数値を生成するのか

ランダムな数値を生成することは、コンピューターサイエンスやプログラミングにおいて非常に重要です。例えば、ゲーム開発や暗号化、シミュレーションなど、数値をランダムに生成することが必要になるケースがあります。また、ランダムな数値を使用することで、プログラムをよりリアルなものにすることができます。

# 生成法

C++でランダムな数値を生成するには、標準ライブラリである `<random>` を使用します。まず、乱数エンジンを作成し、それをもとに分布を指定してランダムな数値を生成します。以下に、整数の場合と実数の場合の例を示します。

```C++
//整数の場合
#include <iostream>
#include <stdlib.h>
#include <random>

int main() {
    //乱数エンジンの作成
    std::mt19937 eng{std::random_device{}()};
    //分布の指定
    std::uniform_int_distribution<int> dist{1, 10}; //1から10までの整数を生成

    //10回ループを回し、ランダムな数値を出力
    for(int i = 0; i < 10; i++){
        std::cout << dist(eng) << std::endl;
    }

    return 0;
}

/* 出力例
5
7
10
2
1
9
3
8
4
6
*/
```

```C++
//実数の場合
#include <iostream>
#include <stdlib.h>
#include <random>

int main() {
    //乱数エンジンの作成
    std::mt19937 eng{std::random_device{}()};
    //分布の指定
    std::uniform_real_distribution<double> dist{0.0, 1.0}; //0から1までの実数を生成

    //10回ループを回し、ランダムな数値を出力
    for(int i = 0; i < 10; i++){
        std::cout << dist(eng) << std::endl;
    }

    return 0;
}

/* 出力例
0.269957
0.478128
0.993718
0.304541
0.153624
0.938067
0.625743
0.02629
0.127482
0.938778
*/
```

# 深堀り

乱数生成アルゴリズムにはいくつかの種類があり、それぞれに独自の特徴があります。C++では、Mersenne Twisterアルゴリズムを使用した乱数エンジンが標準となっており、良質なランダムな数値を生成することができます。

また、分布にもいくつかの種類があり、 `uniform_int_distribution` や `uniform_real_distribution` 以外にも、正規分布や二項分布など様々な分布を指定することができます。これらの分布を使用することで、より現実的な乱数を生成することができます。

# 参考リンク

- [C++ Reference: <random>](https://en.cppreference.com/w/cpp/numeric/random)
- [C++ 範囲ベースforループ、unifor_int_distribution で乱数生成](https://cpprefjp.github.io/lang/cpp11/range_based_for_loop.html)
- [C++で乱数を生成する方法](https://dev.classmethod.jp/etc/cpp_random/)
- [乱数生成アルゴリズムの紹介](https://www.suzu6.net/posts/215-random-algorithm/)