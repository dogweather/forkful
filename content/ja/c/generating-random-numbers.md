---
title:                "「ランダムな数字の生成」"
html_title:           "C: 「ランダムな数字の生成」"
simple_title:         "「ランダムな数字の生成」"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何が目的？ 
ランダムな数字を生成することは、プログラマーがプログラムで使用するランダムなデータを作成することです。例えば、ゲームの乱数生成やセキュリティー目的の暗号鍵生成などに使われます。

## 使い方：
ランダムな数字を生成する方法はいくつかありますが、ここでは代表的な方法を紹介します。

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
  //乱数の種を設定する
  srand(time(NULL));

  //1から10までのランダムな数字を生成する
  int num = rand() % 10 + 1;

  printf("ランダムな数字は%dです。\n", num);

  return 0;
}
```

上記のコードでは、ランダムな数字を生成するために`rand()`関数を使用し、それを1〜10の範囲に調整するために`%`演算子を使っています。さらに、実行するたびに異なる結果を得るために、`srand()`関数を使用して乱数の種を設定しています。

## 深く掘り下げる
ランダムな数字を生成するには様々な方法がありますが、よく使われる方法には疑似乱数生成アルゴリズムがあります。これは、乱数の種を元に計算された数列を生成する方法です。また、ランダムな値が必要な場合に、乱数ではなくハードウェアの乱数ジェネレーターを使用することもできます。

## 関連情報を見る 
- [乱数生成アルゴリズムの説明](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)
- [ハードウェアの乱数ジェネレーターの種類と使い方](https://www.techopedia.com/definition/28661/hardware-random-number-generator-hwrng)