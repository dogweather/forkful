---
date: 2024-01-20 17:45:18.442816-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:38:42.046587-06:00'
model: gpt-4-1106-preview
summary: ":string`\u30AF\u30E9\u30B9\u306E`substr`\u95A2\u6570\u3067\u5B9F\u73FE\u3057\
  \u307E\u3059\u3002\u3053\u308C\u306F\u521D\u7248\u306EC++\u6A19\u6E96\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u306B\u542B\u307E\u308C\u3066\u3044\u307E\u3057\u305F\u3002\u4EE3\
  \u66FF\u624B\u6BB5\u3068\u3057\u3066`std::string_view`\u3084C\u8A00\u8A9E\u306E\
  `strncpy`\u95A2\u6570\u3082\u8003\u3048\u3089\u308C\u307E\u3059\u304C\u3001\u5229\
  \u4FBF\u6027\u3084\u5B89\u5168\u6027\u3067`substr`\u304C\u512A\u308C\u3066\u3044\
  \u307E\u3059\u3002`substr`\u306F\u3001\u6307\u5B9A\u3057\u305F\u958B\u59CB\u4F4D\
  \u7F6E\u3068\u9577\u3055\u306B\u57FA\u3065\u3044\u3066\u65B0\u3057\u3044\u6587\u5B57\
  \u5217\u3092\u751F\u6210\u3057\u307E\u3059\u304C\u3001\u30E1\u30E2\u30EA\u306E\u30B3\
  \u30D4\u30FC\u304C\u95A2\u4E0E\u3059\u308B\u305F\u3081\u3001\u5927\u304D\u306A\u6587\
  \u5B57\u5217\u3067\u306E\u64CD\u4F5C\u306B\u306F\u6CE8\u610F\u304C\u5FC5\u8981\u3067\
  \u3059\u3002"
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
