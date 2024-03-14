---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:08.506685-07:00
description: "\u5927\u5199\u5316\u5B57\u7B26\u4E32\u6D89\u53CA\u5C06\u5B57\u7B26\u4E32\
  \u4E2D\u6BCF\u4E2A\u5355\u8BCD\u7684\u521D\u59CB\u5B57\u7B26\uFF08\u5982\u679C\u5B83\
  \u662F\u5C0F\u5199\u7684\uFF09\u8F6C\u6362\u4E3A\u5927\u5199\uFF0C\u540C\u65F6\u4FDD\
  \u6301\u5176\u4F59\u5B57\u7B26\u4E0D\u53D8\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\
  \u884C\u6B64\u4EFB\u52A1\uFF0C\u7528\u4E8E\u683C\u5F0F\u5316\u8F93\u51FA\u3001\u7528\
  \u6237\u8F93\u5165\u6216\u6570\u636E\u5904\u7406\uFF0C\u4EE5\u786E\u4FDD\u6587\u672C\
  \u7684\u5448\u73B0\u6216\u5904\u7406\u65B9\u5F0F\u4FDD\u6301\u4E00\u81F4\u6027\uFF0C\
  \u7279\u522B\u662F\u5728\u7528\u6237\u754C\u9762\u6216\u6570\u636E\u89C4\u8303\u5316\
  \u4EFB\u52A1\u4E2D\u3002"
lastmod: '2024-03-13T22:44:48.091849-06:00'
model: gpt-4-0125-preview
summary: "\u5927\u5199\u5316\u5B57\u7B26\u4E32\u6D89\u53CA\u5C06\u5B57\u7B26\u4E32\
  \u4E2D\u6BCF\u4E2A\u5355\u8BCD\u7684\u521D\u59CB\u5B57\u7B26\uFF08\u5982\u679C\u5B83\
  \u662F\u5C0F\u5199\u7684\uFF09\u8F6C\u6362\u4E3A\u5927\u5199\uFF0C\u540C\u65F6\u4FDD\
  \u6301\u5176\u4F59\u5B57\u7B26\u4E0D\u53D8\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\
  \u884C\u6B64\u4EFB\u52A1\uFF0C\u7528\u4E8E\u683C\u5F0F\u5316\u8F93\u51FA\u3001\u7528\
  \u6237\u8F93\u5165\u6216\u6570\u636E\u5904\u7406\uFF0C\u4EE5\u786E\u4FDD\u6587\u672C\
  \u7684\u5448\u73B0\u6216\u5904\u7406\u65B9\u5F0F\u4FDD\u6301\u4E00\u81F4\u6027\uFF0C\
  \u7279\u522B\u662F\u5728\u7528\u6237\u754C\u9762\u6216\u6570\u636E\u89C4\u8303\u5316\
  \u4EFB\u52A1\u4E2D\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
---

{{< edit_this_page >}}

## 什么 & 为什么？
大写化字符串涉及将字符串中每个单词的初始字符（如果它是小写的）转换为大写，同时保持其余字符不变。程序员经常执行此任务，用于格式化输出、用户输入或数据处理，以确保文本的呈现或处理方式保持一致性，特别是在用户界面或数据规范化任务中。

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
