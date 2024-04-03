---
date: 2024-01-20 17:45:18.442816-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u90E8\u5206\u6587\u5B57\u5217\u3092\u53D6\
  \u308A\u51FA\u3059\u3053\u3068\u306F\u3001\u6307\u5B9A\u3057\u305F\u7BC4\u56F2\u306E\
  \u6587\u5B57\u5217\u3092\u629C\u304D\u51FA\u3059\u884C\u70BA\u3067\u3059\u3002\u3053\
  \u308C\u306B\u3088\u308A\u7279\u5B9A\u306E\u60C5\u5831\u3092\u51E6\u7406\u3057\u3084\
  \u3059\u304F\u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\u89E3\u6790\u3057\u3084\
  \u3059\u304F\u306A\u308A\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.535431-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u90E8\u5206\u6587\u5B57\u5217\u3092\u53D6\
  \u308A\u51FA\u3059\u3053\u3068\u306F\u3001\u6307\u5B9A\u3057\u305F\u7BC4\u56F2\u306E\
  \u6587\u5B57\u5217\u3092\u629C\u304D\u51FA\u3059\u884C\u70BA\u3067\u3059\u3002\u3053\
  \u308C\u306B\u3088\u308A\u7279\u5B9A\u306E\u60C5\u5831\u3092\u51E6\u7406\u3057\u3084\
  \u3059\u304F\u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\u89E3\u6790\u3057\u3084\
  \u3059\u304F\u306A\u308A\u307E\u3059\u3002."
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## How to (方法)
```C++
#include <iostream>
#include <string>

int main() {
    std::string sentence = "こんにちは、C++の世界へようこそ！";
    std::string greeting = sentence.substr(0, 5); // はじめの5文字を抜き出す
    
    std::cout << greeting << std::endl; // 出力: こんにちは

    std::string welcome = sentence.substr(6); // 6文字目から最後まで抜き出す
    
    std::cout << welcome << std::endl; // 出力: C++の世界へようこそ！
}
```

## Deep Dive (深掘り)
C++での部分文字列抽出は、`std::string`クラスの`substr`関数で実現します。これは初版のC++標準ライブラリに含まれていました。代替手段として`std::string_view`やC言語の`strncpy`関数も考えられますが、利便性や安全性で`substr`が優れています。`substr`は、指定した開始位置と長さに基づいて新しい文字列を生成しますが、メモリのコピーが関与するため、大きな文字列での操作には注意が必要です。

## See Also (関連情報)
- C++ Reference `std::string::substr`: https://en.cppreference.com/w/cpp/string/basic_string/substr
- C++ Reference `std::string_view`: https://en.cppreference.com/w/cpp/string/basic_string_view
- CPP `std::string` Guide: https://www.cplusplus.com/reference/string/string/
