---
date: 2024-01-20 17:38:09.700185-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5C06\u5B57\u7B26\u4E32\u8F6C\
  \u6362\u6210\u5C0F\u5199\u53EF\u4EE5\u4F7F\u7528 `<algorithm>` \u5934\u6587\u4EF6\
  \u4E2D\u7684 `std::transform()` \u51FD\u6570\u3002\u770B\u4E0B\u9762\u7684\u4F8B\
  \u5B50\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.095938-06:00'
model: gpt-4-1106-preview
summary: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u6210\u5C0F\u5199\u53EF\u4EE5\u4F7F\
  \u7528 `<algorithm>` \u5934\u6587\u4EF6\u4E2D\u7684 `std::transform()` \u51FD\u6570\
  \u3002\u770B\u4E0B\u9762\u7684\u4F8B\u5B50\uFF1A."
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

## How to: (如何操作：)
将字符串转换成小写可以使用 `<algorithm>` 头文件中的 `std::transform()` 函数。看下面的例子：

```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string data = "Hello World!";
    std::transform(data.begin(), data.end(), data.begin(),
        [](unsigned char c){ return std::tolower(c); });

    std::cout << data << std::endl; // 输出: hello world!
    return 0;
}
```

这段代码会输出全部小写的字符串 "hello world!"。

## Deep Dive (深入探讨)
早期C++语言中，处理字符串大小写转换可能需要手动遍历每个字符。后来，随着标准库的发展，`<algorithm>` 头文件提供了 `std::transform()` 函数简化这个过程。其他方法包括使用C语言风格的 `std::tolower` 函数逐个字符转换。

注意 `std::tolower` 使用 `unsigned char` 类型来避免负值的字符可能导致的未定义行为。如果需要考虑国际化和本土化需求，你可能需要使用 `std::locale` 类和 `std::use_facet`。

## See Also (另请参阅)
- C++ Reference for `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- C++ Reference for `std::tolower`: https://en.cppreference.com/w/cpp/string/byte/tolower
- C++ Locale library: https://en.cppreference.com/w/cpp/locale
- StackOverflow discussion on string case conversion: https://stackoverflow.com/questions/313970/how-to-convert-an-stdstring-to-lower-case
