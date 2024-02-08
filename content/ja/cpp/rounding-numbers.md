---
title:                "数値の丸め処理"
aliases:
- ja/cpp/rounding-numbers.md
date:                  2024-01-26T03:44:05.670439-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/rounding-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
数値を四捨五入するとは、値を最も近い整数または指定された精度に調整することです。開発者がこれを行う理由は、単純化、現実世界の制約に合わせる、または余分な精度を省いてパフォーマンスを向上させるためです。

## 方法：
C++には、`floor()`、`ceil()`、`round()`など、数値を四捨五入するいくつかの方法があります：

```C++
#include <iostream>
#include <cmath> // 四捨五入関数用

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // 出力：floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // 出力：ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // 出力：round: 3

    // 二桁の小数点まで四捨五入する場合：
    double precise_num = 3.146;
    double multiplier = 100.0;
    double rounded = std::round(precise_num * multiplier) / multiplier;

    std::cout << "二桁の小数点まで四捨五入: " << rounded << "\n"; // 出力：二桁の小数点まで四捨五入: 3.15

    return 0;
}
```

## 徹底解説
C++11以前では、四捨五入は手動技術や非標準ライブラリに依存していました。今日では、`<cmath>`が堅牢な方法を提供します。`floor()`は下へ丸め、`ceil()`は上へ丸め、`round()`は最も近い整数へ丸め、タイブレーキング（0.5のケース）も偶数への丸めで処理します。

これらの関数の動作を理解することは重要です。例えば、負の数ではつまずきがあり得ます(`std::round(-2.5)`は`-2.0`を返します)。

代替案？正の数に対して0.5を足してからintへキャストする古典的なハックがありましたが、負の数で誤りが発生し、型に依存しないわけではありません。Boostのようなライブラリはより繊細なアプローチを提供でき、言語拡張やコンパイラの組み込み機能は特定のハードウェアに対して最適化することができます。

## 参照
- `<cmath>`のC++リファレンス：https://en.cppreference.com/w/cpp/header/cmath
- 浮動小数点演算のIEEE標準（IEEE 754）：https://ieeexplore.ieee.org/document/4610935
- Boost 数値変換ライブラリ：https://www.boost.org/doc/libs/release/libs/numeric/conversion/
