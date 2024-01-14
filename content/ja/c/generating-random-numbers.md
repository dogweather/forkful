---
title:    "C: ランダムな数値の生成"
keywords: ["C"]
---

{{< edit_this_page >}}

## なぜランダムな数値を生成するのか？

ランダムな数値を生成することは、プログラミングにおいて非常に便利なツールです。例えば、ゲームやシミュレーション、セキュリティなど様々な分野で使用されています。ランダムな数値を生成することにより、プログラムは毎回同じ結果を出さず、よりリアルなものを作ることができます。

## 生成方法

ランダムな数値を生成するためには、C言語には標準ライブラリとして rand() 関数が用意されています。これを使用することで、整数型のランダムな数値を取得することができます。以下は、1から10までのランダムな数値を3回出力するサンプルコードです。

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
  // シード値の設定 (今回は固定値)
  srand(1234);
  
  int i;
  for (i = 0; i < 3; i++) {
    // 1から10までのランダムな数値を取得
    int num = rand() % 10 + 1;
    printf("%d\n", num);
  }
  
  return 0;
}
```

上記のコードを実行すると、以下のような出力が得られます。

```
7
10
2
```

rand() は毎回異なる数値を取得するため、実行するたびに出力結果が変わることがわかります。

## 深堀り

ランダムな数値を生成する際、シード値の設定が重要です。シード値とは、rand() が乱数を生成する際に使用する初期値のことを指します。先ほどのサンプルコードでは、固定値の 1234 をシード値として設定しましたが、このままだと毎回同じ結果を出力してしまいます。

そのため、通常は実行時刻をシード値として指定することが推奨されています。これにより、異なる実行時刻からは異なるシード値が生成されるため、よりランダムな数値を取得することができます。

また、rand() は擬似乱数を生成するため、完全にランダムではありません。そのため、セキュリティなどの分野では別の方法でランダムな数値を生成する必要があります。

## 同様の記事

- [How to Generate Random Numbers in C](https://www.programiz.com/c-programming/examples/generate-random)
- [Random number generation in C](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Generating Random numbers in C programming](https://beginnersbook.com/2014/01/c-generating-random-numbers/)

## 関連リンク

- [rand()関数の仕様](https://ja.wikipedia.org/wiki/Rand)
- [C言語リファレンス - srand()関数](https://www.tutorialspoint.com/c_standard_library/c_function_srand.htm)
- [C言語ドットコム - rand()とsrand()で乱数を発生させる](https://www.c-lang-engineering.com/news/c_functions_rand_and_srand/)