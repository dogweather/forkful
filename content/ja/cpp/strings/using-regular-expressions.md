---
title:                "正規表現の使用"
aliases:
- /ja/cpp/using-regular-expressions/
date:                  2024-02-03T19:16:12.600055-07:00
model:                 gpt-4-0125-preview
simple_title:         "正規表現の使用"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
