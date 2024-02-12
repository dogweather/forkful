---
title:                "使用正则表达式"
aliases:
- /zh/cpp/using-regular-expressions.md
date:                  2024-02-03T19:16:10.496593-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用正则表达式"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
在C++中，正则表达式是定义搜索模式的字符序列，用于字符串匹配或操作。程序员使用它们来执行诸如验证输入、搜索字符串中的出现，或将字符串分解为标记等任务，使它们成为高效有效的文本处理的不可或缺的工具。

## 如何做：
C++11在标准库`<regex>`中引入了对正则表达式的支持，提供了一个健壮的字符串搜索和操作框架。这里是一个使用正则表达式在字符串内搜索模式的基本示例：

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target = "Hello, my email is example@example.com";
    std::regex email_pattern(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(target, email_pattern)) {
        std::cout << "发现电子邮件!" << std::endl;
    } else {
        std::cout << "未发现电子邮件。" << std::endl;
    }

    return 0;
}
```
**样例输出**
```
发现电子邮件!
```

对于更复杂的操作，例如在字符串中替换模式，C++的正则表达式可以非常方便：

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
**样例输出**
```
Th* r**n *n Sp**n f*lls m**nly *n th* pl**n.
```

对于那些探索标准库之外的程序员来说，Boost Regex库（`boost/regex.hpp`）是一个受欢迎的第三方选项，提供增强的正则表达式能力和性能优化，尤其是对于复杂模式或大量数据处理：

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "Boost libraries are fun!";
    boost::regex expr("(\\w+)\\s(libraries)"); // 匹配“Boost libraries”
    std::string fmt("GNU \\1"); // 替换为“GNU Boost”

    std::string result = boost::regex_replace(s, expr, fmt);
    std::cout << result << std::endl;

    return 0;
}
```
**样例输出**
```
GNU Boost are fun!
```

这些示例仅展示了C++用正则表达式能力的表皮，说明了基本搜索、模式匹配和替换的用法，无论是使用标准库还是由Boost的强大正则实现增强。
