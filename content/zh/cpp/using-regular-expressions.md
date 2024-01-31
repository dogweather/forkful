---
title:                "使用正则表达式"
date:                  2024-01-19
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"

category:             "C++"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
正则表达式是个强大的文本处理工具，用于搜索、替换、检查模式。程序员用它来处理复杂的文本任务，快速又省力。

## How to: (如何操作)
```cpp
#include <iostream>
#include <regex>
#include <string>

int main() {
    std::string text = "程序设计很有趣!";
    std::regex vowelRegex("[aeiouAEIOU]");

    // 替换所有的英文元音字母为星号
    std::string replacedText = std::regex_replace(text, vowelRegex, "*");
    std::cout << replacedText << std::endl;

    return 0;
}
```
输出：
```
程序设计很有趣!
```

## Deep Dive (深入探索)
正则表达式起源于20世纪50年代的自动机理论。C++在`<regex>`库中提供正则表达式支持。除了正则表达式，字符串搜索可以用find函数等简单方法，但不如正则强大。实现细节上，编译器用有限状态机来解析和执行模式。

## See Also (另请参阅)
- [C++ `<regex>` documentation](http://www.cplusplus.com/reference/regex/)
- [Regex101: Online regex tester and debugger](https://regex101.com/)
- [Regular Expressions - Modern C++](https://en.cppreference.com/w/cpp/regex)
