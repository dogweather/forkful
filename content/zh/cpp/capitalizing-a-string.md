---
title:                "字符串首字母大写"
date:                  2024-01-19
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"

category:             "C++"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (什么及为何？)
字符串大写是把字母转换为大写格式。程序员这样做主要为了标准化文本输入输出，比如人名、地点或编程中的常量。

## How to: (如何操作：)
```C++
#include <iostream>
#include <algorithm>
#include <cctype>

int main() {
    std::string str = "welcome to C++ programming!";
    std::transform(str.begin(), str.end(), str.begin(),
                   [](unsigned char c){ return std::toupper(c); });
    
    std::cout << str << std::endl; // 输出：WELCOME TO C++ PROGRAMMING!
    return 0;
}
```

## Deep Dive (深入了解)
过去，大写字符串对于旧式打印机和计算机显示器很有必要，以免混淆小写字母。替代方法包括手动遍历字符串逐个字符转换，或者使用库函数如 `boost::to_upper`。在实际实现时，考虑到不同语言的大写规则，正确大写化可能需要更复杂的库，比如`locale`。

## See Also (另请参阅)
- [cplusplus.com - std::toupper](http://www.cplusplus.com/reference/cctype/toupper/)
- [cppreference.com - std::transform](https://en.cppreference.com/w/cpp/algorithm/transform)
- [boost.org - String Algorithms](https://www.boost.org/doc/libs/1_75_0/doc/html/string_algo.html)
