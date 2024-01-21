---
title:                "ランダム数の生成"
date:                  2024-01-20T17:48:38.305663-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ランダムな数字を生成するってのは、予測不可能な数を生み出すことだ。プログラムに予期せぬ行動やテスト、シミュレーションに真剣みを加えるために使うね。

## How to: (方法)
```C++
#include <iostream>
#include <random>

int main() {
    // ランダムエンジンと分布を初期化
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distrib(1, 100);

    // ランダムな数を生成
    int random_number = distrib(gen);
    
    // 出力
    std::cout << "生成されたランダムな数: " << random_number << std::endl;
    
    return 0;
}
```

このコードを実行すると、例えば「生成されたランダムな数: 42」と出力されるかもしれない。毎回異なる結果が得られる。

## Deep Dive (深掘り)
昔は乱数生成に`srand`と`rand`関数をよく使ったけど、予測しにくく高品質な乱数が必要な現代では、`<random>`ヘッダ内のメルセンヌ・ツイスター（`std::mt19937`）のようなアルゴリズムが好まれる。他のアルゴリズムには`std::linear_congruential_engine`や`std::subtract_with_carry_engine`がある。各アルゴリズムの数学的背景や性能についてはドキュメントを参照してほしい。また、`std::random_device`を使うことで、ハードウェアレベルのエントロピーを取り込んでより予測不可能な乱数シードを確保できる。

## See Also (関連情報)
- C++公式ドキュメント内`<random>`: https://en.cppreference.com/w/cpp/header/random
- C++メルセンヌ・ツイスターについて: https://en.cppreference.com/w/cpp/numeric/random/mersenne_twister_engine
- 乱数生成アルゴリズムのパフォーマンスに関する研究: https://www.jstatsoft.org/article/view/v008i14