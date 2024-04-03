---
date: 2024-01-20 17:34:35.464211-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.539963-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

## How to: (方法)
```cpp
#include <iostream>
#include <string>

int main() {
    std::string greeting = "こんにちは、";
    std::string name = "世界!";
    std::string message = greeting + name; // 文字列を連結

    std::cout << message << std::endl; // 結果を表示
    return 0;
}
```
実行結果: `こんにちは、世界!`

## Deep Dive (深い潜入)
文字列の連結はC++の初期バージョンから存在します。`std::string`クラスの`+`演算子を使うのが一般的で、これにより複数の`std::string`オブジェクトが連結できます。古いスタイルでは、Cスタイルの文字列で`strcat()`関数を使用する方法もありましたが、バッファオーバーフローの危険があります。`std::stringstream`やフォーマットされた出力も使えます。パフォーマンスを考えると、文字列の連結はコストが高い操作であることも覚えておく必要があります。

## See Also (参照)
- C++ Standard Library: https://cplusplus.com/reference/string/string/
- `std::stringstream`の使用: https://cplusplus.com/reference/sstream/stringstream/
- モダンC++のフォーマットライブラリ(fmtlib): https://fmt.dev/latest/index.html
