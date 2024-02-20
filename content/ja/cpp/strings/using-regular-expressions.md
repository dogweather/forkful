---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:12.600055-07:00
description: "C++\u3067\u306E\u6B63\u898F\u8868\u73FE\u306F\u3001\u6587\u5B57\u5217\
  \u306E\u691C\u7D22\u30D1\u30BF\u30FC\u30F3\u3092\u5B9A\u7FA9\u3059\u308B\u6587\u5B57\
  \u306E\u4E26\u3073\u3067\u3042\u308A\u3001\u6587\u5B57\u5217\u306E\u4E00\u81F4\u691C\
  \u7D22\u3084\u64CD\u4F5C\u306E\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5165\u529B\u306E\u691C\u8A3C\
  \u3001\u6587\u5B57\u5217\u5185\u306E\u51FA\u73FE\u7B87\u6240\u306E\u691C\u7D22\u3001\
  \u6587\u5B57\u5217\u306E\u30C8\u30FC\u30AF\u30F3\u5316\u306A\u3069\u306E\u30BF\u30B9\
  \u30AF\u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\u3001\u52B9\u7387\u7684\u304B\
  \u3064\u52B9\u679C\u7684\u306A\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u306E\u305F\u3081\
  \u306E\u4E0D\u53EF\u6B20\u306A\u30C4\u30FC\u30EB\u3068\u3057\u3066\u3044\u307E\u3059\
  \u3002"
lastmod: 2024-02-19 22:05:01.650836
model: gpt-4-0125-preview
summary: "C++\u3067\u306E\u6B63\u898F\u8868\u73FE\u306F\u3001\u6587\u5B57\u5217\u306E\
  \u691C\u7D22\u30D1\u30BF\u30FC\u30F3\u3092\u5B9A\u7FA9\u3059\u308B\u6587\u5B57\u306E\
  \u4E26\u3073\u3067\u3042\u308A\u3001\u6587\u5B57\u5217\u306E\u4E00\u81F4\u691C\u7D22\
  \u3084\u64CD\u4F5C\u306E\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5165\u529B\u306E\u691C\u8A3C\u3001\
  \u6587\u5B57\u5217\u5185\u306E\u51FA\u73FE\u7B87\u6240\u306E\u691C\u7D22\u3001\u6587\
  \u5B57\u5217\u306E\u30C8\u30FC\u30AF\u30F3\u5316\u306A\u3069\u306E\u30BF\u30B9\u30AF\
  \u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\u3001\u52B9\u7387\u7684\u304B\u3064\
  \u52B9\u679C\u7684\u306A\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u306E\u305F\u3081\u306E\
  \u4E0D\u53EF\u6B20\u306A\u30C4\u30FC\u30EB\u3068\u3057\u3066\u3044\u307E\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？
C++での正規表現は、文字列の検索パターンを定義する文字の並びであり、文字列の一致検索や操作のために使用されます。プログラマーは、入力の検証、文字列内の出現箇所の検索、文字列のトークン化などのタスクにそれらを使用し、効率的かつ効果的なテキスト処理のための不可欠なツールとしています。

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
