---
title:                "標準エラーへの書き込み"
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
標準エラー出力（stderr）は、エラーメッセージや診断情報を表示します。これは、正常な出力（stdout）とエラーを分け、ログやユーザーに直接伝えるために使用します。

## How to: (方法)
```C++
#include <iostream>

int main() {
    std::cerr << "エラーが発生しました" << std::endl;
    // 通常の出力
    std::cout << "通常の出力" << std::endl;
    return 0;
}
```
サンプル出力:
```
エラーが発生しました
通常の出力
```

## Deep Dive (深掘り)
標準エラー出力はUNIXシステムの初期から存在します。`std::cerr`はバッファリングされていないので、`std::clog`や`std::cout`に比べて即時性があります。プログラムがクラッシュした場合でも、エラーメッセージは失われにくいです。実装において、`std::cerr`は`std::ostream`クラスを継承しており、標準出力と同じ操作を行うことができますが、目的が異なります。

## See Also (関連情報)
- [cppreference.com - std::cerr](https://en.cppreference.com/w/cpp/io/cerr)
- [C++ Standard Library Reference](http://www.cplusplus.com/reference/)
- [GNU C++ Library Documentation](https://gcc.gnu.org/onlinedocs/libstdc++/latest-doxygen/a01576.html)
