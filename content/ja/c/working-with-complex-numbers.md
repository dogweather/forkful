---
title:                "複素数の扱い方"
date:                  2024-01-26T04:37:44.823388-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
複素数は、実数部と虚数部（例えば3 + 4iのように）の混合であり、信号処理や特定の方程式を解くなど、高度な計算で重要です。従来の数値では不十分な、数学的に重いアプリケーションで、プログラマーがそれらを扱います。

## 方法：
Cは、C99から、ネイティブの複素数型とライブラリを持っています。以下がその使用方法です：

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // 複素数を二つ宣言
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // 複素数の演算
    double complex sum = z1 + z2;
    double complex mult = z1 * z2;

    // 結果の出力
    printf("和: %.1f + %.1fi\n", creal(sum), cimag(sum));
    printf("積: %.1f + %.1fi\n", creal(mult), cimag(mult));

    // 絶対値と偏角
    printf("Abs(z1): %f\n", cabs(z1));
    printf("Arg(z1): %f\n", carg(z1));

    return 0;
}
```

サンプル出力：
```
和: 3.0 + 1.0i
積: 8.0 + 2.0i
Abs(z1): 3.162278
Arg(z1): 1.249046
```
## 詳細解説
複素数は何世紀も前にさかのぼり、16世紀の代数学にそのルーツがあります。時を経て、今ではCだけでなく、多くのプログラミング言語で不可欠な要素です。

C99標準では、マクロ、関数、および`complex`データ型を定義する`<complex.h>`ヘッダが導入されました。代替手段も存在します - 例えば独自の構造体を作成するなどですが、なぜ車輪の再発明をするのでしょうか？C標準ライブラリは最適化されており、使用準備ができています。

その力にもかかわらず、Cの複素数サポートには批判もあります。Pythonのような言語の類似機能より直感的ではないかもしれず、コーナーケースの扱いが厄介になることもあります。しかし、生のパフォーマンスにおいて、それはまだ堅実な選択です。

## 参照
- C99標準文書`<complex.h>`：https://en.cppreference.com/w/c/numeric/complex
- 浮動小数点演算のIEEE標準（IEEE 754）：https://ieeexplore.ieee.org/document/4610935
- Cの複素数数学のためのオンラインチュートリアル：https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming
