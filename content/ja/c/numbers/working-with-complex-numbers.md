---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:58.215094-07:00
description: "\u8907\u7D20\u6570\u306F\u5B9F\u90E8\u3068\u865A\u90E8\u304B\u3089\u69CB\
  \u6210\u3055\u308C\u3001\u300Ca + bi\u300D\u3068\u8868\u3055\u308C\u307E\u3059\u3002\
  \u3053\u3053\u3067\u3001`i`\u306F`-1`\u306E\u5E73\u65B9\u6839\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u96FB\u6C17\u5DE5\u5B66\u3001\u91CF\u5B50\
  \u30B3\u30F3\u30D4\u30E5\u30FC\u30C6\u30A3\u30F3\u30B0\u3001\u6D41\u4F53\u529B\u5B66\
  \u306A\u3069\u69D8\u3005\u306A\u5206\u91CE\u3067\u8907\u7D20\u6570\u3092\u6271\u3044\
  \u3001\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\u30F3\u3001\u4FE1\u53F7\u51E6\u7406\
  \u3001\u7279\u5B9A\u306E\u7A2E\u985E\u306E\u6570\u5B66\u65B9\u7A0B\u5F0F\u306E\u89E3\
  \u6C7A\u306B\u5F7C\u3089\u306E\u30E6\u30CB\u30FC\u30AF\u306A\u7279\u6027\u3092\u6D3B\
  \u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.726255-07:00'
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u5B9F\u90E8\u3068\u865A\u90E8\u304B\u3089\u69CB\
  \u6210\u3055\u308C\u3001\u300Ca + bi\u300D\u3068\u8868\u3055\u308C\u307E\u3059\u3002\
  \u3053\u3053\u3067\u3001`i`\u306F`-1`\u306E\u5E73\u65B9\u6839\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u96FB\u6C17\u5DE5\u5B66\u3001\u91CF\u5B50\
  \u30B3\u30F3\u30D4\u30E5\u30FC\u30C6\u30A3\u30F3\u30B0\u3001\u6D41\u4F53\u529B\u5B66\
  \u306A\u3069\u69D8\u3005\u306A\u5206\u91CE\u3067\u8907\u7D20\u6570\u3092\u6271\u3044\
  \u3001\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\u30F3\u3001\u4FE1\u53F7\u51E6\u7406\
  \u3001\u7279\u5B9A\u306E\u7A2E\u985E\u306E\u6570\u5B66\u65B9\u7A0B\u5F0F\u306E\u89E3\
  \u6C7A\u306B\u5F7C\u3089\u306E\u30E6\u30CB\u30FC\u30AF\u306A\u7279\u6027\u3092\u6D3B\
  \u7528\u3057\u307E\u3059\u3002"
title: "\u8907\u7D20\u6570\u3092\u64CD\u4F5C\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

複素数は実部と虚部から構成され、「a + bi」と表されます。ここで、`i`は`-1`の平方根です。プログラマーは、電気工学、量子コンピューティング、流体力学など様々な分野で複素数を扱い、シミュレーション、信号処理、特定の種類の数学方程式の解決に彼らのユニークな特性を活用します。

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
