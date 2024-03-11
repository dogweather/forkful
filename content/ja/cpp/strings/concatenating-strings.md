---
date: 2024-01-20 17:34:35.464211-07:00
description: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\u8907\u6570\u306E\
  \u30C6\u30AD\u30B9\u30C8\u7247\u3092\u4E00\u3064\u306B\u7E4B\u3052\u308B\u3053\u3068\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u8868\
  \u793A\u3001\u30E1\u30C3\u30BB\u30FC\u30B8\u306E\u7D44\u307F\u7ACB\u3066\u3001\u30D5\
  \u30A1\u30A4\u30EB\u306E\u30D1\u30B9\u306A\u3069\u3001\u591A\u69D8\u306A\u7406\u7531\
  \u304B\u3089\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:16.086824-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\u8907\u6570\u306E\
  \u30C6\u30AD\u30B9\u30C8\u7247\u3092\u4E00\u3064\u306B\u7E4B\u3052\u308B\u3053\u3068\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u8868\
  \u793A\u3001\u30E1\u30C3\u30BB\u30FC\u30B8\u306E\u7D44\u307F\u7ACB\u3066\u3001\u30D5\
  \u30A1\u30A4\u30EB\u306E\u30D1\u30B9\u306A\u3069\u3001\u591A\u69D8\u306A\u7406\u7531\
  \u304B\u3089\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
---

{{< edit_this_page >}}

## What & Why? (なぜか？)
文字列の連結とは、複数のテキスト片を一つに繋げることです。プログラマーはデータ表示、メッセージの組み立て、ファイルのパスなど、多様な理由からこれを行います。

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
