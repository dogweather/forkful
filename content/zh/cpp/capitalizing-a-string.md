---
title:                "将字符串变为大写"
html_title:           "C++: 将字符串变为大写"
simple_title:         "将字符串变为大写"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么&为什么?

把字符串转化为大写（capitalize a string）就是把字符串的所有字符都变成大写字母。程序员这么做是为了在诸如用户输入、文件处理等场景保持数据的一致性。

## 如何实现:

在C++中，你可以这样做:

```C++
#include <algorithm>
#include <string>
#include <iostream>

int main() {
    std::string s = "hello";
    std::transform(s.begin(), s.end(), s.begin(), ::toupper);

    std::cout << s;
}
```

运行此代码后，您将看到输出为"HELLO"。

## 深入了解

(1) 在早期的编程语言中，字符的大小写转换并不总是一个简单的任务，有时要依赖于特定的ASCII表。现在，我们的现代语言如C++已经提供了标准库来完成这个任务。

(2) 除了使用::toupper和std::transform之外，你也可以使用现代C++中更灵活的`for_each`算法。他们之间的主要区别在于transform在每个元素上应用一个函数并将结果存回容器，而for_each则仅在每个元素上应用函数，不改变容器中的元素。

(3) 当使用::toupper时，输入必须是一个unsigned char或者EOF，其它输入可能导致未定义的行为。事实上，::toupper是一个以当前locale为参数的宏，它返回对应大写字母的值，如果输入不是小写字母则返回输入本身。

## 参考资料

- C++ Reference关于[toupper](https://en.cppreference.com/w/cpp/string/byte/toupper)函数的详细介绍。
- Stack Overflow关于C++ [capitalizing strings](https://stackoverflow.com/questions/735204/convert-a-string-in-c-to-upper-case)的讨论。
- 关于C++[algorithms](https://www.cplusplus.com/reference/algorithm/)库的深入教程。