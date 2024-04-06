---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:21.629398-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A C++\u3067\u306F\u3001\
  \u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u3066\u6587\u5B57\
  \u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\
  \u3059\u304C\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u306F\u5FC5\u8981\u3042\u308A\u307E\u305B\u3093\u3002\u305F\u3060\u3057\
  \u3001\u3088\u308A\u8907\u96D1\u307E\u305F\u306F\u7279\u5B9A\u306E\u5927\u6587\u5B57\
  \u5316\u306E\u632F\u308B\u821E\u3044\u306B\u3064\u3044\u3066\u306F\u3001Boost\u306E\
  \u3088\u3046\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u975E\u5E38\u306B\u5F79\u7ACB\
  \u3064\u3053\u3068\u304C\u3042\u308A\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u4E21\u65B9\
  \u306E\u30A2\u30D7\u30ED\u30FC\u30C1\u3092\u8AAC\u660E\u3059\u308B\u4F8B\u3092\u793A\
  \u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.527265-06:00'
model: gpt-4-0125-preview
summary: "C++\u3067\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\
  \u7528\u3057\u3066\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u304C\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\
  \u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u5FC5\u8981\u3042\u308A\u307E\u305B\
  \u3093\u3002\u305F\u3060\u3057\u3001\u3088\u308A\u8907\u96D1\u307E\u305F\u306F\u7279\
  \u5B9A\u306E\u5927\u6587\u5B57\u5316\u306E\u632F\u308B\u821E\u3044\u306B\u3064\u3044\
  \u3066\u306F\u3001Boost\u306E\u3088\u3046\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\
  \u975E\u5E38\u306B\u5F79\u7ACB\u3064\u3053\u3068\u304C\u3042\u308A\u307E\u3059\u3002\
  \u4EE5\u4E0B\u306B\u4E21\u65B9\u306E\u30A2\u30D7\u30ED\u30FC\u30C1\u3092\u8AAC\u660E\
  \u3059\u308B\u4F8B\u3092\u793A\u3057\u307E\u3059."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## どのようにして：
C++では、標準ライブラリを使用して文字列を大文字化することができますが、サードパーティのライブラリは必要ありません。ただし、より複雑または特定の大文字化の振る舞いについては、Boostのようなライブラリが非常に役立つことがあります。以下に両方のアプローチを説明する例を示します。

### 標準C++ライブラリを使用する：
```cpp
#include <iostream>
#include <cctype> // std::tolower and std::toupper用
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // 出力: "Hello World From C++"
}
```

### Boostライブラリを使用する：
より高度な文字列操作、ロケール認識の大文字化を含む場合、Boost String Algoライブラリの使用を検討するかもしれません。

まず、プロジェクトでBoostライブラリがインストールされ、設定されていることを確認してください。次に、必要なヘッダを含め、以下に示すようにその機能を使用できます。

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // 各単語の最初の文字を大文字にする
    boost::algorithm::to_lower(capitalizedText); // 文字列を小文字にすることを保証する
    capitalizedText[0] = std::toupper(capitalizedText[0]); // 最初の文字を大文字にする

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // スペースの後に大文字にする
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // 出力: "Hello World From C++"
}
```

この場合、Boostは文字列操作のタスクをいくらか簡素化しますが、主に変換とケース変換のユーティリティを提供しているため、真の大文字化にはカスタムアプローチがまだ必要です。
