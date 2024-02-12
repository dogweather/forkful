---
title:                "获取字符串的长度"
aliases: - /zh/cpp/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:03.572907-07:00
model:                 gpt-4-1106-preview
simple_title:         "获取字符串的长度"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么和为什么？)
找出字符串的长度是指确定字符串中字符的数量。程序员这么做是为了数据验证、循环逻辑、内存分配等操作。

## How to (如何操作):
```C++
#include <iostream>
#include <string>

int main() {
    std::string greeting = "你好";
    std::cout << "The length of the string is: " << greeting.length() << std::endl;
    return 0;
}
```

输出:
```
The length of the string is: 2
```

## Deep Dive (深入了解):
字符串长度的计算在编程历史中一直很重要。它经常用于字符串操作中，如切割、序列化、反序列化。

在 C++ 中，`std::string` 类型提供 `.length()` 和 `.size()`，两者功能相同，都可以返回字符串长度。

早期的 C 风格字符串使用 `\0` 作为结束符，长度是手动计算的，通过遍历字符串直到结束符。

除了 `std::string` 类型，C++ STL（Standard Template Library）也提供了一些替代方案，比如 `std::basic_string` 可以工作在不同的字符类型上（如宽字符）。使用 `std::wstring` 计算宽字符字符串的长度时，注意编码和平台的差异可能会影响长度计算。

## See Also (另请参看):
- C++ `std::string` 类参考：[http://www.cplusplus.com/reference/string/string/](http://www.cplusplus.com/reference/string/string/)
- C++ 风格字符串（C-strings）及其处理方式：[http://www.cplusplus.com/reference/cstring/](http://www.cplusplus.com/reference/cstring/)
