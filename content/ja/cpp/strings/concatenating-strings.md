---
date: 2024-01-20 17:34:35.464211-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.430544-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6587\u5B57\u5217\u306E\u9023\u7D50\u306FC++\u306E\u521D\
  \u671F\u30D0\u30FC\u30B8\u30E7\u30F3\u304B\u3089\u5B58\u5728\u3057\u307E\u3059\u3002\
  `std::string`\u30AF\u30E9\u30B9\u306E`+`\u6F14\u7B97\u5B50\u3092\u4F7F\u3046\u306E\
  \u304C\u4E00\u822C\u7684\u3067\u3001\u3053\u308C\u306B\u3088\u308A\u8907\u6570\u306E\
  `std::string`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u304C\u9023\u7D50\u3067\u304D\u307E\
  \u3059\u3002\u53E4\u3044\u30B9\u30BF\u30A4\u30EB\u3067\u306F\u3001C\u30B9\u30BF\u30A4\
  \u30EB\u306E\u6587\u5B57\u5217\u3067`strcat()`\u95A2\u6570\u3092\u4F7F\u7528\u3059\
  \u308B\u65B9\u6CD5\u3082\u3042\u308A\u307E\u3057\u305F\u304C\u3001\u30D0\u30C3\u30D5\
  \u30A1\u30AA\u30FC\u30D0\u30FC\u30D5\u30ED\u30FC\u306E\u5371\u967A\u304C\u3042\u308A\
  \u307E\u3059\u3002`std::stringstream`\u3084\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3055\
  \u308C\u305F\u51FA\u529B\u3082\u4F7F\u3048\u307E\u3059\u3002\u30D1\u30D5\u30A9\u30FC\
  \u30DE\u30F3\u30B9\u3092\u8003\u3048\u308B\u3068\u3001\u6587\u5B57\u5217\u306E\u9023\
  \u7D50\u306F\u30B3\u30B9\u30C8\u304C\u9AD8\u3044\u64CD\u4F5C\u3067\u3042\u308B\u3053\
  \u3068\u3082\u899A\u3048\u3066\u304A\u304F\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\
  \u3002"
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
