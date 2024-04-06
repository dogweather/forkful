---
date: 2024-01-20 17:47:19.655175-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.756327-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u77E5\u308B\u6A5F\
  \u80FD\u306F\u3001C++\u306E\u521D\u671F\u304B\u3089\u5B58\u5728\u3057\u307E\u3059\
  \u3002C\u8A00\u8A9E\u3067\u306F`strlen`\u95A2\u6570\u304C\u4F7F\u308F\u308C\u3066\
  \u3044\u307E\u3057\u305F\u304C\u3001C++\u3067\u306F`std::string`\u30AF\u30E9\u30B9\
  \u3067`.length()`\u3084`.size()`\u30E1\u30BD\u30C3\u30C9\u304C\u63D0\u4F9B\u3055\
  \u308C\u3066\u3044\u307E\u3059\u3002\u3053\u308C\u3089\u306E\u30E1\u30BD\u30C3\u30C9\
  \u306F\u3001\u540C\u3058\u7D50\u679C\u3092\u8FD4\u3057\u307E\u3059\u304C\u3001\u9069\
  \u7528\u3059\u308B\u6587\u8108\u306B\u3088\u3063\u3066\u4F7F\u3044\u5206\u3051\u308B\
  \u3053\u3068\u304C\u63A8\u5968\u3055\u308C\u307E\u3059\uFF08`.length()`\u306F\u6587\
  \u5B57\u5217\u306E\u9577\u3055\u304C\u3001`.size()`\u306F\u30B3\u30F3\u30C6\u30CA\
  \u306E\u5927\u304D\u3055\u304C\u7126\u70B9\u306E\u6642\u306B\u4F7F\u3044\u307E\u3059\
  \uFF09."
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

## How to: (方法)
```C++
#include <iostream>
#include <string>

int main() {
    std::string myString = "こんにちは世界";
    std::cout << "The length of the string is: " << myString.length() << std::endl;
    // C++11以降、以下のようにも書けます
    std::cout << "The length of the string is: " << myString.size() << std::endl;
    return 0;
}
```
出力:
```
The length of the string is: 21
The length of the string is: 21
```

## Deep Dive (詳細な解説)
文字列の長さを知る機能は、C++の初期から存在します。C言語では`strlen`関数が使われていましたが、C++では`std::string`クラスで`.length()`や`.size()`メソッドが提供されています。これらのメソッドは、同じ結果を返しますが、適用する文脈によって使い分けることが推奨されます（`.length()`は文字列の長さが、`.size()`はコンテナの大きさが焦点の時に使います）。

内部的には、`std::string`は文字の配列を管理しつつ、長さの情報を追跡することで高速に長さを報告します。これは計算の必要がなく、効率的です。

他にも、C++スタイルでは`std::string`を使いますが、Cスタイルの文字列（NULLで終了する文字配列）でも長さは`strlen`関数で取得可能です。しかし、安全性や使い勝手で`std::string`が推奨されます。

## See Also (関連情報)
- [std::string::length - cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string/length)
- [std::string::size - cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string/size)
- [C++ Strings (std::string)](https://www.cplusplus.com/reference/string/string/)
