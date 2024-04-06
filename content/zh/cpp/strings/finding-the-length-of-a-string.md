---
date: 2024-01-20 17:47:03.572907-07:00
description: "How to (\u5982\u4F55\u64CD\u4F5C): \u5B57\u7B26\u4E32\u957F\u5EA6\u7684\
  \u8BA1\u7B97\u5728\u7F16\u7A0B\u5386\u53F2\u4E2D\u4E00\u76F4\u5F88\u91CD\u8981\u3002\
  \u5B83\u7ECF\u5E38\u7528\u4E8E\u5B57\u7B26\u4E32\u64CD\u4F5C\u4E2D\uFF0C\u5982\u5207\
  \u5272\u3001\u5E8F\u5217\u5316\u3001\u53CD\u5E8F\u5217\u5316\u3002 \u5728 C++ \u4E2D\
  \uFF0C`std::string` \u7C7B\u578B\u63D0\u4F9B `.length()` \u548C `.size()`\uFF0C\u4E24\
  \u8005\u529F\u80FD\u76F8\u540C\uFF0C\u90FD\u53EF\u4EE5\u8FD4\u56DE\u5B57\u7B26\u4E32\
  \u957F\u5EA6\u3002 \u65E9\u671F\u7684 C \u98CE\u683C\u5B57\u7B26\u4E32\u4F7F\u7528\
  \ `\\0`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.256048-06:00'
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u957F\u5EA6\u7684\u8BA1\u7B97\u5728\u7F16\u7A0B\u5386\
  \u53F2\u4E2D\u4E00\u76F4\u5F88\u91CD\u8981\u3002\u5B83\u7ECF\u5E38\u7528\u4E8E\u5B57\
  \u7B26\u4E32\u64CD\u4F5C\u4E2D\uFF0C\u5982\u5207\u5272\u3001\u5E8F\u5217\u5316\u3001\
  \u53CD\u5E8F\u5217\u5316\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

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
