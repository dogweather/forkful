---
title:                "字符串拼接"
aliases:
- /zh/cpp/concatenating-strings.md
date:                  2024-01-20T17:34:29.771795-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串拼接"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/concatenating-strings.md"
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
