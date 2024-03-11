---
date: 2024-01-20 17:38:09.700185-07:00
description: "\u8F6C\u6362\u5B57\u7B26\u4E32\u5230\u5C0F\u5199\u662F\u628A\u6240\u6709\
  \u5B57\u6BCD\u6539\u6210\u5C0F\u5199\u5F62\u5F0F\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u6570\u636E\u7EDF\u4E00\u5316\
  \uFF0C\u6BD4\u5982\u5728\u6BD4\u8F83\u5B57\u7B26\u4E32\u65F6\u5FFD\u7565\u5927\u5C0F\
  \u5199\u5DEE\u5F02\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.896177-06:00'
model: gpt-4-1106-preview
summary: "\u8F6C\u6362\u5B57\u7B26\u4E32\u5230\u5C0F\u5199\u662F\u628A\u6240\u6709\
  \u5B57\u6BCD\u6539\u6210\u5C0F\u5199\u5F62\u5F0F\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u6570\u636E\u7EDF\u4E00\u5316\
  \uFF0C\u6BD4\u5982\u5728\u6BD4\u8F83\u5B57\u7B26\u4E32\u65F6\u5FFD\u7565\u5927\u5C0F\
  \u5199\u5DEE\u5F02\u3002"
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
转换字符串到小写是把所有字母改成小写形式的过程。程序员这么做通常是为了数据统一化，比如在比较字符串时忽略大小写差异。

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
