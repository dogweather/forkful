---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:08.506685-07:00
description: "\u5982\u4F55\u505A\uFF1A \u5728 C++ \u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\
  \u7528\u6807\u51C6\u5E93\u6765\u5927\u5199\u5316\u5B57\u7B26\u4E32\uFF0C\u65E0\u9700\
  \u7B2C\u4E09\u65B9\u5E93\u3002\u7136\u800C\uFF0C\u5BF9\u4E8E\u66F4\u590D\u6742\u6216\
  \u7279\u5B9A\u7684\u5927\u5199\u5316\u884C\u4E3A\uFF0C\u50CF Boost \u8FD9\u6837\u7684\
  \u5E93\u53EF\u80FD\u975E\u5E38\u6709\u5E2E\u52A9\u3002\u4E0B\u9762\u7684\u793A\u4F8B\
  \u8BF4\u660E\u4E86\u4E24\u79CD\u65B9\u6CD5\u3002 #."
lastmod: '2024-03-13T22:44:48.091849-06:00'
model: gpt-4-0125-preview
summary: "\u5728 C++ \u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u6807\u51C6\u5E93\u6765\
  \u5927\u5199\u5316\u5B57\u7B26\u4E32\uFF0C\u65E0\u9700\u7B2C\u4E09\u65B9\u5E93\u3002\
  \u7136\u800C\uFF0C\u5BF9\u4E8E\u66F4\u590D\u6742\u6216\u7279\u5B9A\u7684\u5927\u5199\
  \u5316\u884C\u4E3A\uFF0C\u50CF Boost \u8FD9\u6837\u7684\u5E93\u53EF\u80FD\u975E\u5E38\
  \u6709\u5E2E\u52A9\u3002\u4E0B\u9762\u7684\u793A\u4F8B\u8BF4\u660E\u4E86\u4E24\u79CD\
  \u65B9\u6CD5."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何做：
在 C++ 中，你可以使用标准库来大写化字符串，无需第三方库。然而，对于更复杂或特定的大写化行为，像 Boost 这样的库可能非常有帮助。下面的示例说明了两种方法。

### 使用标准 C++ 库：
```cpp
#include <iostream>
#include <cctype> // 用于 std::tolower 和 std::toupper
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
    std::cout << capitalizedText << std::endl; // 输出："Hello World From C++"
}
```

### 使用 Boost 库：
对于更高级的字符串操作，包括考虑语言环境的大写化，你可能想使用 Boost String Algo 库。

首先，确保你已经在项目中安装并配置了 Boost 库。然后你可以包含必要的头文件，并按下面所示使用其功能。

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // 大写化每个单词的第一个字母
    boost::algorithm::to_lower(capitalizedText); // 确保字符串处于小写状态
    capitalizedText[0] = std::toupper(capitalizedText[0]); // 大写化第一个字符

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // 在空格后大写化
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // 输出："Hello World From C++"
}
```

在这个例子中，Boost 简化了一些字符串操作任务，但仍然需要自定义方法来实现真正的大写化，因为它主要提供转换和大小写转换工具。
