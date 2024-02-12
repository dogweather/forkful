---
title:                "複素数を操作する"
aliases: - /ja/c/working-with-complex-numbers.md
date:                  2024-02-03T18:13:58.215094-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数を操作する"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-complex-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
