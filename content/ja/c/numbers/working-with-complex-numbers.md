---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:58.215094-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A C\u8A00\u8A9E\u3067\
  \u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA `<complex.h>` \u306B\u3088\
  \u3063\u3066\u8907\u7D20\u6570\u304C\u30B5\u30DD\u30FC\u30C8\u3055\u308C\u3066\u3044\
  \u307E\u3059\u3002\u3053\u308C\u3092\u5229\u7528\u3059\u308B\u306B\u306F\u3001`double\
  \ complex`\u578B\uFF08\u307E\u305F\u306F\u5358\u7CBE\u5EA6\u7528\u306E`float complex`\uFF09\
  \u3067\u5909\u6570\u3092\u5BA3\u8A00\u3057\u307E\u3059\u3002\u57FA\u672C\u7684\u306A\
  \u64CD\u4F5C\u306E\u65B9\u6CD5\u306F\u4EE5\u4E0B\u306E\u3068\u304A\u308A\u3067\u3059\
  \uFF1A."
lastmod: '2024-04-05T21:53:43.570437-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u8907\u7D20\u6570\u3092\u64CD\u4F5C\u3059\u308B"
weight: 14
---

## どのようにして：
C言語では、標準ライブラリ `<complex.h>` によって複素数がサポートされています。これを利用するには、`double complex`型（または単精度用の`float complex`）で変数を宣言します。基本的な操作の方法は以下のとおりです：

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // 複素数 1+2i を宣言
    double complex z2 = 1.0 - 2.0*I; // 別の複素数 1-2i を宣言

    // 加算
    double complex sum = z1 + z2;
    printf("Sum: %.2f + %.2fi\n", creal(sum), cimag(sum)); // 出力: Sum: 2.00 + 0.00i

    // 乗算
    double complex product = z1 * z2;
    printf("Product: %.2f + %.2fi\n", creal(product), cimag(product)); // 出力: Product: 5.00 + 0.00i

    // 複素共役
    double complex conjugate = conj(z1);
    printf("Conjugate of z1: %.2f + %.2fi\n", creal(conjugate), cimag(conjugate)); // 出力: Conjugate of z1: 1.00 - 2.00i

    // 絶対値
    double magnitude = cabs(z1);
    printf("Magnitude of z1: %.2f\n", magnitude); // 出力: Magnitude of z1: 2.24

    // 位相
    double phase = carg(z1);
    printf("Phase of z1: %.2f\n", phase); // ラジアン単位での出力

    return 0;
}
```
`I`は `<complex.h>` において虚数単位を表す定数です。`creal()`や`cimag()`のような関数はそれぞれ実部と虚部を抽出し、`conj()`は複素共役を計算します。複素数の絶対値と位相（引数）には、`cabs()`と`carg()`が使用されます。

## ディープダイブ
Cにおける複素数のサポートは比較的最近のもので、C99で標準化されました。それ以前は、C言語における複素数算術は煩雑で、しばしばカスタムデータ構造や関数が必要でした。`<complex.h>`や複素データ型の導入は、科学的および工学的応用における言語の能力を大きく向上させました。しかし、Pythonのような言語は、組み込みデータ型とより豊富なライブラリ機能を通じて、複素数に対するより直感的なサポートを提供していることに注意すべきです。これにもかかわらず、Cが提供する性能と制御は、複素数算術の若干煩雑な構文を扱いつつも、高性能コンピューティングタスクにおける選択肢として好まれています。
