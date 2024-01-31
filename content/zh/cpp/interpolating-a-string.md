---
title:                "字符串插值"
date:                  2024-01-20T17:50:31.335921-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串插值"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
字符插值是将变量或表达式的值嵌入到字符串中的过程。程序员这样做是为了动态构建字符串，使输出更加灵活和有用。

## How to: (如何实现：)
在C++中，你可以使用`std::ostringstream`或C++20中的`std::format`来插值字符串。

```C++
#include <iostream>
#include <sstream>
#include <format>

int main() {
    // Using std::ostringstream
    std::ostringstream oss;
    int age = 25;
    oss << "我今年" << age << "岁。";
    std::cout << oss.str() << std::endl;

    // Using std::format (C++20)
    std::string result = std::format("我今年{}岁。", age);
    std::cout << result << std::endl;

    return 0;
}
```

输出:
```
我今年25岁。
我今年25岁。
```

## Deep Dive (深入探索)
在C++中，字符串插值不是像一些其他语言那样直接内置的功能。早期，需要利用流（如`std::ostringstream`）手动构建字符串。随着C++20的到来，`std::format`提供了一种类似于Python风格的字符串格式化方法，它更简洁和高效。`std::format`背后使用的是格式字符串语法（Format String Syntax），这改善了字符串的可读性和维护性。替代方法还包括使用`sprintf`或字符串连接，但它们可能不如`std::format`安全或方便。

## See Also (另请参阅)
- [C++20 std::format documentation](https://en.cppreference.com/w/cpp/utility/format)
- [C++ string handling FAQ](http://www.comeaucomputing.com/faq/strings/)
- [C++ ostringstream reference](https://www.cplusplus.com/reference/sstream/ostringstream/)
