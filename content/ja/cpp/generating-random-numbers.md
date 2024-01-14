---
title:    "C++: 乱数を生成する"
keywords: ["C++"]
---

{{< edit_this_page >}}

## なぜランダムな数字を生成するのか

ランダムな数字を生成することは、多くのプログラミング言語で重要な機能です。ランダムな数字を使用することで、ゲームやシミュレーションなどのプログラムをよりリアルにすることができます。また、ランダムな数字を使用することで、偏りのない公平な結果を得ることができます。

## 生成方法

ランダムな数字を生成するには、C++の標準ライブラリである```<random>```を使用します。以下のコードは、0から99までのランダムな数字を生成し、その結果を表示する例です。

```C++
#include <iostream>
#include <random>

int main() {
  // 乱数生成器を初期化する
  std::random_device rd;
  std::mt19937 gen(rd());

  // 0から99までの乱数を生成する
  std::uniform_int_distribution<int> dist(0, 99);
  int random_num = dist(gen);

  // 結果を表示する
  std::cout << "ランダムな数字: " << random_num << std::endl;

  return 0;
}
```

このコードの結果は、例えば「ランダムな数字: 58」となります。```<random>```ライブラリにはさまざまな乱数生成関数が用意されており、プログラムの目的や必要性に応じて使用する関数を選ぶことができます。

## 詳細な説明

ランダムな数字を生成するには、擬似乱数生成器を使用します。擬似乱数生成器は、初期値(シード)を元に一定の計算を繰り返し、次々と乱数を生成します。そのため、同じ初期値を使うと同じ乱数が生成されます。そのため、プログラムの実行順や初期値を変えることによって異なる乱数列を生成することができます。

また、本当のランダムではなく、計算で生成されるため、完全なランダムとは言えない場合があります。そのため、暗号学的なセキュリティを必要とする場合には、専用の擬似乱数生成器を使用します。

## さらに学ぼう

まだランダムな数字生成について学びたい場合は、以下のリソースを参考にしてください。

- [C++の標準ライブラリにおける乱数生成](https://cpprefjp.github.io/reference/random.html)
- [擬似乱数生成器について](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [暗号学的なセキュリティを必要とする場合の擬似乱数生成器](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)

See Also（参考情報）：
- [C++の標準ライブラリについて学ぶ](https://www.cplusplus.com/reference/)
- [C++における基本的な乱数生成方法についてのチュートリアル](https://www.geeksforgeeks.org/generating-random-number-cpp/)