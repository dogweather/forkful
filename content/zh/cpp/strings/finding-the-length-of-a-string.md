---
date: 2024-01-20 17:47:03.572907-07:00
description: "\u627E\u51FA\u5B57\u7B26\u4E32\u7684\u957F\u5EA6\u662F\u6307\u786E\u5B9A\
  \u5B57\u7B26\u4E32\u4E2D\u5B57\u7B26\u7684\u6570\u91CF\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u662F\u4E3A\u4E86\u6570\u636E\u9A8C\u8BC1\u3001\u5FAA\u73AF\u903B\u8F91\
  \u3001\u5185\u5B58\u5206\u914D\u7B49\u64CD\u4F5C\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.100355-06:00'
model: gpt-4-1106-preview
summary: "\u627E\u51FA\u5B57\u7B26\u4E32\u7684\u957F\u5EA6\u662F\u6307\u786E\u5B9A\
  \u5B57\u7B26\u4E32\u4E2D\u5B57\u7B26\u7684\u6570\u91CF\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u662F\u4E3A\u4E86\u6570\u636E\u9A8C\u8BC1\u3001\u5FAA\u73AF\u903B\u8F91\
  \u3001\u5185\u5B58\u5206\u914D\u7B49\u64CD\u4F5C\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

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
