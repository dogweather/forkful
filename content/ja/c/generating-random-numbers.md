---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？

乱数生成とは、何らかの一貫性なく値を発生させるプロセスのことです。これは、ゲームの中で事象をランダムに発生させたり、暗号化に必要なキーを生成したりするためにプログラマーが行います。

## 実施方法：

以下は、Cプログラムの中で乱数を生成する基本的な方法です：

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
   // シード値初期化
   srand(time(0));

   // 生成した乱数を出力
   printf("%d\n", rand());
   
   return 0;
}
```

上記コードの出力は、ある一貫性のない数値です。つまり、それは乱数です。

## 深堀り：

乱数生成の初めての主流な方針は、1960年代に発表されました。現在、C言語の`rand()`関数と`srand()`関数を使用するのが一般的です。他方、より高度な乱数生成のニーズには、`random()`や`drand48()`関数が使用されますが、これらは`rand()`よりも少し複雑です。

生成される乱数は、シーケンスを開始する「シード」値に依存します。一般に、シード値として現在の時刻を使用することが多いです。これは`time(0)`が返す値になります。

## 参照資料：

- C言語乱数の生成について更に学びたい方は、以下のリンクをご参照ください： https://www.ibm.com/docs/ja/i/7.4?topic=ssw_ibm_i_74/rzarg/rand.htm
- 別のアプローチとして`random()`関数について学びたい方は、こちらのリンクが有用です： https://www.gnu.org/software/libc/manual/html_node/Simple-Random.html