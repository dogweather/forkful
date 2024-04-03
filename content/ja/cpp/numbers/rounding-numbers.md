---
date: 2024-01-26 03:44:05.670439-07:00
description: "\u65B9\u6CD5\uFF1A C++\u306B\u306F\u3001`floor()`\u3001`ceil()`\u3001\
  `round()`\u306A\u3069\u3001\u6570\u5024\u3092\u56DB\u6368\u4E94\u5165\u3059\u308B\
  \u3044\u304F\u3064\u304B\u306E\u65B9\u6CD5\u304C\u3042\u308A\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.544172-06:00'
model: gpt-4-0125-preview
summary: "C++\u306B\u306F\u3001`floor()`\u3001`ceil()`\u3001`round()`\u306A\u3069\u3001\
  \u6570\u5024\u3092\u56DB\u6368\u4E94\u5165\u3059\u308B\u3044\u304F\u3064\u304B\u306E\
  \u65B9\u6CD5\u304C\u3042\u308A\u307E\u3059\uFF1A."
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

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
