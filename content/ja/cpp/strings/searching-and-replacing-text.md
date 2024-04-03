---
date: 2024-01-20 17:57:13.838156-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\
  \u3042\u308B\u6587\u5B57\u5217\u3092\u5225\u306E\u6587\u5B57\u5217\u3067\u7F6E\u304D\
  \u63DB\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u30D0\u30B0\u4FEE\u6B63\u3001\u30B3\u30FC\u30C9\u66F4\u65B0\u3001\u30C7\u30FC\
  \u30BF\u5909\u63DB\u306A\u3069\u306E\u305F\u3081\u306B\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.530273-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\
  \u3042\u308B\u6587\u5B57\u5217\u3092\u5225\u306E\u6587\u5B57\u5217\u3067\u7F6E\u304D\
  \u63DB\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u30D0\u30B0\u4FEE\u6B63\u3001\u30B3\u30FC\u30C9\u66F4\u65B0\u3001\u30C7\u30FC\
  \u30BF\u5909\u63DB\u306A\u3069\u306E\u305F\u3081\u306B\u884C\u3044\u307E\u3059\u3002\
  ."
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
