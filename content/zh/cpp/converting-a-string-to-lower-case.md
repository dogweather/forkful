---
title:                "将字符串转换为小写"
date:                  2024-01-20T17:38:09.700185-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/converting-a-string-to-lower-case.md"
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