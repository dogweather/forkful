---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:12.600055-07:00
description: "\u4F7F\u3044\u65B9: C++11\u3067\u306F\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\
  \u30EA`<regex>`\u306B\u3066\u6B63\u898F\u8868\u73FE\u306E\u30B5\u30DD\u30FC\u30C8\
  \u304C\u5C0E\u5165\u3055\u308C\u3001\u6587\u5B57\u5217\u691C\u7D22\u3084\u64CD\u4F5C\
  \u306E\u305F\u3081\u306E\u5805\u7262\u306A\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\
  \u304C\u63D0\u4F9B\u3055\u308C\u307E\u3057\u305F\u3002\u4EE5\u4E0B\u306F\u3001\u6587\
  \u5B57\u5217\u5185\u3067\u30D1\u30BF\u30FC\u30F3\u3092\u691C\u7D22\u3059\u308B\u305F\
  \u3081\u306E\u6B63\u898F\u8868\u73FE\u306E\u57FA\u672C\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.536928-06:00'
model: gpt-4-0125-preview
summary: "C++11\u3067\u306F\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA`<regex>`\u306B\
  \u3066\u6B63\u898F\u8868\u73FE\u306E\u30B5\u30DD\u30FC\u30C8\u304C\u5C0E\u5165\u3055\
  \u308C\u3001\u6587\u5B57\u5217\u691C\u7D22\u3084\u64CD\u4F5C\u306E\u305F\u3081\u306E\
  \u5805\u7262\u306A\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u304C\u63D0\u4F9B\u3055\
  \u308C\u307E\u3057\u305F\u3002\u4EE5\u4E0B\u306F\u3001\u6587\u5B57\u5217\u5185\u3067\
  \u30D1\u30BF\u30FC\u30F3\u3092\u691C\u7D22\u3059\u308B\u305F\u3081\u306E\u6B63\u898F\
  \u8868\u73FE\u306E\u57FA\u672C\u4F8B\u3067\u3059\uFF1A."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 使い方:
C++11では標準ライブラリ`<regex>`にて正規表現のサポートが導入され、文字列検索や操作のための堅牢なフレームワークが提供されました。以下は、文字列内でパターンを検索するための正規表現の基本例です：

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target = "Hello, my email is example@example.com";
    std::regex email_pattern(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(target, email_pattern)) {
        std::cout << "Email found!" << std::endl;
    } else {
        std::cout << "No email found." << std::endl;
    }

    return 0;
}
```
**サンプル出力**
```
Email found!
```

文字列内のパターンを置き換えるなど、より複雑な操作においても、C++の正規表現は非常に便利です：

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string text = "The rain in Spain falls mainly in the plain.";
    std::regex vowel_regex("([aeiou])");

    std::string replaced_text = std::regex_replace(text, vowel_regex, "*");
    std::cout << replaced_text << std::endl;

    return 0;
}
```
**サンプル出力**
```
Th* r**n *n Sp**n f*lls m**nly *n th* pl**n.
```

標準ライブラリを超えて探求するプログラマーにとって、Boost Regexライブラリ（`boost/regex.hpp`）は、特に複雑なパターンや広範なデータ処理において、強化された正規表現機能とパフォーマンスの最適化を提供する人気のサードパーティオプションです：

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "Boost libraries are fun!";
    boost::regex expr("(\\w+)\\s(libraries)"); // "Boost libraries" に一致する
    std::string fmt("GNU \\1"); // "GNU Boost" に置き換える

    std::string result = boost::regex_replace(s, expr, fmt);
    std::cout << result << std::endl;

    return 0;
}
```
**サンプル出力**
```
GNU Boost are fun!
```

これらの例は、標準ライブラリを使用してもBoostの強力な正規表現実装によっても、基本的な検索、パターンマッチング、置き換えを行うC++の正規表現を用いた機能の表面をかい摘んでいます。
