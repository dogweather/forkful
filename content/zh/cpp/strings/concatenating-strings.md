---
aliases:
- /zh/cpp/concatenating-strings/
date: 2024-01-20 17:34:29.771795-07:00
description: "\u5B57\u7B26\u4E32\u7684\u8FDE\u63A5\u662F\u5C06\u4E24\u4E2A\u6216\u591A\
  \u4E2A\u5B57\u7B26\u4E32\u9996\u5C3E\u76F8\u8FDE\u5F62\u6210\u4E00\u4E2A\u65B0\u5B57\
  \u7B26\u4E32\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u4E3B\u8981\
  \u662F\u4E3A\u4E86\u751F\u6210\u52A8\u6001\u5185\u5BB9\uFF0C\u6BD4\u5982\u7ED3\u5408\
  \u59D3\u540D\u751F\u6210\u5168\u540D\uFF0C\u6216\u6784\u5EFA\u6709\u610F\u4E49\u7684\
  \u6D88\u606F\u548C\u7528\u6237\u63A5\u53E3\u6587\u672C\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.398387
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u7684\u8FDE\u63A5\u662F\u5C06\u4E24\u4E2A\u6216\u591A\
  \u4E2A\u5B57\u7B26\u4E32\u9996\u5C3E\u76F8\u8FDE\u5F62\u6210\u4E00\u4E2A\u65B0\u5B57\
  \u7B26\u4E32\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u4E3B\u8981\
  \u662F\u4E3A\u4E86\u751F\u6210\u52A8\u6001\u5185\u5BB9\uFF0C\u6BD4\u5982\u7ED3\u5408\
  \u59D3\u540D\u751F\u6210\u5168\u540D\uFF0C\u6216\u6784\u5EFA\u6709\u610F\u4E49\u7684\
  \u6D88\u606F\u548C\u7528\u6237\u63A5\u53E3\u6587\u672C\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)

字符串的连接是将两个或多个字符串首尾相连形成一个新字符串的过程。程序员这样做主要是为了生成动态内容，比如结合姓名生成全名，或构建有意义的消息和用户接口文本。

## How to: (如何操作：)

```C++
#include <iostream>
#include <string>

int main() {
    std::string firstName = "张";
    std::string lastName = "三";
    std::string fullName = firstName + lastName; // 使用 + 操作符连接字符串

    std::cout << "完整姓名: " << fullName << std::endl;

    // C++11 后，可以使用 += 或者 append() 进行字符串拼接。
    std::string greeting = "你好, ";
    greeting += fullName; // 或者 greeting.append(fullName);

    std::cout << greeting << std::endl;

    return 0;
}
```
输出：
```
完整姓名: 张三
你好, 张三
```

## Deep Dive (深入了解)

历史上，C++中字符串的拼接一度受限于字符数组的操作，需要使用函数如 `strcat()` 处理。C++的标准模板库（STL）引入`std::string`类后，字符串操作变得简单许多。+操作符重载和`append()`方法更是直观、高效。

其他方法还包括使用字符串流（例如`std::stringstream`），这在需要组合多种数据类型为字符串时特别有用。在性能敏感的应用中，避免多次不必要的字符串复制很重要，因此选择正确的连接方法和预分配足够内存至关重要。

对于实现细节，`std::string`的连接操作会根据需要扩展字符串的内存。但是`std::string::reserve()`方法可以预分配内存以提高效率。而在C++17及以后版本，`std::string_view`提供了一个非拷贝的轻量级字符串参考方式，减少了一些字符串操作的开销。

## See Also (另请参阅)

- C++标准库文档关于 [std::string](http://www.cplusplus.com/reference/string/string/)
- [cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string) 关于更高级的字符串操作方法
- 相关文章: [关于用`std::stringstream`串联不同数据类型](https://www.cprogramming.com/tutorial/string.html)
