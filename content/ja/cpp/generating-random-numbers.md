---
title:                "ランダムな数字の生成"
html_title:           "C++: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ
ランダムな数値を生成することのメリットについて説明します。人々はこの機能を使用して、シミュレーション、ランダムなデータの生成、そして暗号化など、さまざまな目的で利用することができます。

## 使い方
ランダムな数値を生成するためには、C++の標準ライブラリである`<random>`を使用します。以下のコード例を参考に、ランダムな整数を生成する方法をご紹介します。

```C++
#include <iostream>
#include <random>

int main() {
    // ランダムな整数を生成するためのエンジンを作成
    std::random_device rd;
    std::mt19937 gen(rd());

    // 生成される数値の範囲を指定
    std::uniform_int_distribution<> dist(1, 10);

    // 10回ループしてランダムな数値を表示
    for (int i = 0; i < 10; i++) {
        std::cout << dist(gen) << " ";
    }
    std::cout << std::endl;
}
```

上記のコードを実行すると、1から10の範囲でランダムな数値が生成されます。実行するたびに異なる結果が得られることが確認できるでしょう。

## ディープダイブ
ランダムな数値を生成するアルゴリズムには、メルセンヌ・ツイスターやリニア・コングルエンシャル法などの様々な手法が存在します。また、seed(種)を指定することで、同じ結果を得ることも可能です。

特に注意すべき点として、標準ライブラリの`<random>`は疑似乱数を生成するため、完全にランダムな数値を生成することはできません。しかし、一般的な用途では十分な精度でランダムな数値を生成することができます。

## 参考リンク
- [C++ Reference: Random Number Generating](https://en.cppreference.com/w/cpp/numeric/random)
- [C++標準ライブラリの<random>の使い方(日本語ブログ)](https://marycore.jp/prog/cpp/random/)