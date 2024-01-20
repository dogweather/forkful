---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何であり、なぜか？
ランダムな数値を生成するとは、毎回異なる結果を出すプロセスのことです。プログラマはこれを利用して結果の多様性を保つためや、テストデータを生成するために行います。

## 手順
ランダム数値を生成するためのC++のコード例とその出力結果を以下に示します。
```C++
#include <random>
#include <iostream>

int main() {
    // ランダムなエンジンを生成
    std::random_device rd;

    // 0から99までの数を生成する分布を生成
    std::uniform_int_distribution<int> dist(0, 99);

    // ランダムな数を生成
    int random_num = dist(rd);
    
    std::cout << "Generated Random Number: " << random_num << std::endl;

    return 0;
}
```
実行すると、以下のように表示されます。「Generated Random Number: ～」の部分は毎回異なります。
```
Generated Random Number: 53
```

## より深く知るために
以前に使われていたrand()関数は、今日ではstd::random_deviceやstd::uniform_int_distributionの組み合わせが推奨されています。どちらもC++11以降で導入され、前者はより高度なランダム性を提供し、後者は特定の範囲内でのランダムな数を提供します。

代替手段として、nexttowardやfmodなどの関数を用いて自己実装する方法もあります。ただし、これらの関数を使用する場合は、生成される数値のランダム性が十分であるかを確認する必要があります。

ライブラリの実装詳細については、各ライブラリの公式ドキュメンテーションを直接参照するか、出典を確認してください。

## 関連情報
- C++公式ドキュメンテーション（random）: http://en.cppreference.com/w/cpp/numeric/random
- Effective Modern C++（日本語版）: https://www.amazon.co.jp/dp/4774194244/
- ランダム数生成の数学的背景（英語）: https://www.johndcook.com/blog/2016/01/29/random-number-generator-testing/