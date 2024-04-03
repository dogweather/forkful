---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:10.496593-07:00
description: "\u5728C++\u4E2D\uFF0C\u6B63\u5219\u8868\u8FBE\u5F0F\u662F\u5B9A\u4E49\
  \u641C\u7D22\u6A21\u5F0F\u7684\u5B57\u7B26\u5E8F\u5217\uFF0C\u7528\u4E8E\u5B57\u7B26\
  \u4E32\u5339\u914D\u6216\u64CD\u4F5C\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u4EEC\
  \u6765\u6267\u884C\u8BF8\u5982\u9A8C\u8BC1\u8F93\u5165\u3001\u641C\u7D22\u5B57\u7B26\
  \u4E32\u4E2D\u7684\u51FA\u73B0\uFF0C\u6216\u5C06\u5B57\u7B26\u4E32\u5206\u89E3\u4E3A\
  \u6807\u8BB0\u7B49\u4EFB\u52A1\uFF0C\u4F7F\u5B83\u4EEC\u6210\u4E3A\u9AD8\u6548\u6709\
  \u6548\u7684\u6587\u672C\u5904\u7406\u7684\u4E0D\u53EF\u6216\u7F3A\u7684\u5DE5\u5177\
  \u3002"
lastmod: '2024-03-13T22:44:48.099314-06:00'
model: gpt-4-0125-preview
summary: "\u5728C++\u4E2D\uFF0C\u6B63\u5219\u8868\u8FBE\u5F0F\u662F\u5B9A\u4E49\u641C\
  \u7D22\u6A21\u5F0F\u7684\u5B57\u7B26\u5E8F\u5217\uFF0C\u7528\u4E8E\u5B57\u7B26\u4E32\
  \u5339\u914D\u6216\u64CD\u4F5C\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u4EEC\u6765\
  \u6267\u884C\u8BF8\u5982\u9A8C\u8BC1\u8F93\u5165\u3001\u641C\u7D22\u5B57\u7B26\u4E32\
  \u4E2D\u7684\u51FA\u73B0\uFF0C\u6216\u5C06\u5B57\u7B26\u4E32\u5206\u89E3\u4E3A\u6807\
  \u8BB0\u7B49\u4EFB\u52A1\uFF0C\u4F7F\u5B83\u4EEC\u6210\u4E3A\u9AD8\u6548\u6709\u6548\
  \u7684\u6587\u672C\u5904\u7406\u7684\u4E0D\u53EF\u6216\u7F3A\u7684\u5DE5\u5177\u3002\
  ."
title: "\u4F7F\u7528\u6B63\u5219\u8868\u8FBE\u5F0F"
weight: 11
---

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
