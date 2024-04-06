---
date: 2024-01-20 17:57:13.838156-07:00
description: "How to: (\u65B9\u6CD5) \u30C6\u30AD\u30B9\u30C8\u306E\u7F6E\u63DB\u306F\
  \u6B74\u53F2\u7684\u306B\u30A8\u30C7\u30A3\u30BF\u306E\u7F6E\u63DB\u6A5F\u80FD\u3067\
  \u3088\u304F\u7528\u3044\u3089\u308C\u3066\u304D\u307E\u3057\u305F\u3002C++\u3067\
  \u306F `<string>` \u3068 `<algorithm>` \u30D8\u30C3\u30C0\u5185\u306E\u95A2\u6570\
  \u3067\u7F6E\u63DB\u304C\u53EF\u80FD\u3067\u3059\u3002`find()` \u3068 `replace()`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.346070-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u30C6\u30AD\u30B9\u30C8\u306E\u7F6E\u63DB\u306F\u6B74\u53F2\
  \u7684\u306B\u30A8\u30C7\u30A3\u30BF\u306E\u7F6E\u63DB\u6A5F\u80FD\u3067\u3088\u304F\
  \u7528\u3044\u3089\u308C\u3066\u304D\u307E\u3057\u305F\u3002C++\u3067\u306F `<string>`\
  \ \u3068 `<algorithm>` \u30D8\u30C3\u30C0\u5185\u306E\u95A2\u6570\u3067\u7F6E\u63DB\
  \u304C\u53EF\u80FD\u3067\u3059\u3002`find()` \u3068 `replace()` \u306F\u57FA\u672C\
  \u7684\u306A\u30E1\u30BD\u30C3\u30C9\u3067\u3059\u304C\u3001\u6B63\u898F\u8868\u73FE\
  \u3092\u4F7F\u3063\u3066\u8907\u96D1\u306A\u30D1\u30BF\u30FC\u30F3\u306B\u5BFE\u5FDC\
  \u3059\u308B\u3053\u3068\u3082\u3067\u304D\u307E\u3059( `<regex>` \u3092\u53C2\u7167\
  )\u3002`std::regex_replace` \u306F\u691C\u7D22\u3068\u7F6E\u63DB\u3092\u4E00\u3064\
  \u306E\u624B\u9806\u3067\u5B9F\u884C\u3057\u3001\u3088\u308A\u5F37\u529B\u3067\u3059\
  \u3002\u4EE3\u66FF\u3068\u3057\u3066\u3001Boost\u30E9\u30A4\u30D6\u30E9\u30EA\u306A\
  \u3069\u306E\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u304C\u3001C++11\u304B\u3089\u6B63\u898F\
  \u8868\u73FE\u306F\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u4E00\u90E8\u306B\
  \u306A\u308A\u307E\u3057\u305F\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

## How to: (方法)
```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string text = "こんにちは、プログラミングの世界へようこそ!";
    std::string from = "プログラミング";
    std::string to = "C++";

    // 検索と置換
    auto start_pos = text.find(from);
    if(start_pos != std::string::npos) {
        text.replace(start_pos, from.length(), to);
    }

    std::cout << text << std::endl;

    return 0;
}

// 出力: こんにちは、C++の世界へようこそ!
```

## Deep Dive (深掘り)
テキストの置換は歴史的にエディタの置換機能でよく用いられてきました。C++では `<string>` と `<algorithm>` ヘッダ内の関数で置換が可能です。`find()` と `replace()` は基本的なメソッドですが、正規表現を使って複雑なパターンに対応することもできます( `<regex>` を参照)。`std::regex_replace` は検索と置換を一つの手順で実行し、より強力です。代替として、Boostライブラリなどのサードパーティライブラリを使用できますが、C++11から正規表現は標準ライブラリの一部になりました。

## See Also (関連情報)
- C++ Reference std::string: [https://en.cppreference.com/w/cpp/string/basic_string](https://en.cppreference.com/w/cpp/string/basic_string)
- C++ Reference std::regex: [https://en.cppreference.com/w/cpp/regex](https://en.cppreference.com/w/cpp/regex)
- Boost String Algorithms Library: [https://www.boost.org/doc/libs/release/libs/algorithm/string/](https://www.boost.org/doc/libs/release/libs/algorithm/string/)
