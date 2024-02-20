---
date: 2024-01-26 04:37:53.072637-07:00
description: "\u8907\u7D20\u6570\u306F\u3001\u865A\u6570\u5358\u4F4D'i'\u3092\u52A0\
  \u3048\u308B\u3053\u3068\u3067\u5B9F\u6570\u3092\u62E1\u5F35\u3057\u307E\u3059\u3002\
  \u3053\u3053\u3067\u3001i^2 = -1\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u30012\u6B21\u5143\u3067\u306E\u4F5C\u696D\u304C\u5FC5\u8981\u306A\u30B7\
  \u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\u30F3\u3001\u4FE1\u53F7\u51E6\u7406\u3001\u6570\
  \u5B66\u554F\u984C\u306E\u89E3\u6C7A\u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\
  \u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.656358
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u3001\u865A\u6570\u5358\u4F4D'i'\u3092\u52A0\u3048\
  \u308B\u3053\u3068\u3067\u5B9F\u6570\u3092\u62E1\u5F35\u3057\u307E\u3059\u3002\u3053\
  \u3053\u3067\u3001i^2 = -1\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u30012\u6B21\u5143\u3067\u306E\u4F5C\u696D\u304C\u5FC5\u8981\u306A\u30B7\u30DF\
  \u30E5\u30EC\u30FC\u30B7\u30E7\u30F3\u3001\u4FE1\u53F7\u51E6\u7406\u3001\u6570\u5B66\
  \u554F\u984C\u306E\u89E3\u6C7A\u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\u307E\
  \u3059\u3002"
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
複素数は、虚数単位'i'を加えることで実数を拡張します。ここで、i^2 = -1です。プログラマーは、2次元での作業が必要なシミュレーション、信号処理、数学問題の解決にそれらを使用します。

## どのようにして：
C++には、複素数を扱うのを容易にする組み込みライブラリ`<complex>`があります。こちらが簡単な見方です：

```cpp
#include <iostream>
#include <complex>

int main() {
    std::complex<double> num1(2.0, 3.0); // 複素数（2 + 3i）を作成
    std::complex<double> num2(3.0, 4.0); // 別の複素数（3 + 4i）

    // 加算
    std::complex<double> result = num1 + num2;
    std::cout << "加算結果: " << result << std::endl; // (5 + 7i)

    // 乗算
    result = num1 * num2;
    std::cout << "乗算結果: " << result << std::endl; // (-6 + 17i)

    // 共役
    result = std::conj(num1);
    std::cout << "num1の共役: " << result << std::endl; // (2 - 3i)
    
    return 0;
}
```

## 深く掘り下げて
複素数には豊かな歴史があり、最初は16世紀の立方方程式の解として登場しました。プログラミングだけでなく、多くの分野で不可欠です。コンピュータ科学内では、2次元の数値空間を必要とするアルゴリズム、例えば高速フーリエ変換（FFT）などで複素数が役立ちます。

C++の`<complex>`ライブラリは標準ですが、Pythonの`complex`データ型やJavaScriptの数学ライブラリなど、他の言語にも代替が存在します。`<complex>`ライブラリ自体は、複素数向けに調整された三角関数、指数関数、対数関数の演算を含む広範な機能を提供します。

これらの数値をプログラミングする際には、不正確さを防ぎ、複素共役などの操作や、複素指数関数を三角関数に関連付けるオイラーの公式の意味を理解するために背後にある数学を把握することが重要です。

## 参照
- C++標準テンプレートライブラリドキュメント：https://en.cppreference.com/w/cpp/header/complex
- 複素数に関するさらなる数学的掘り下げ：https://mathworld.wolfram.com/ComplexNumber.html
- 可視化のために、PythonライブラリのMatplotlibは複素数をプロットできます：https://matplotlib.org/
