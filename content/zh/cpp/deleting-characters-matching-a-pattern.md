---
title:                "删除匹配模式的字符"
html_title:           "Java: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么&为什么？
删除匹配模式的字符是用来移除字符串中特定模式的字符。对程序员而言，这有助于对数据进行清理和格式化。

## 怎么做：
下面是一个案例，可以清楚看出如何使用 `std::erase` 和 `std::remove` 函数来删除匹配模式的字符。

```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string myString = "Hello, World! 123";
    myString.erase(std::remove(myString.begin(), myString.end(), 'o'), myString.end());
    std::cout << myString;
    return 0;
}
```
运行以上代码，你将得到以下输出：

```C++
Hell, Wrld! 123
```
如你所见，所有的 'o' 字符都已被删除。

## 深入瞭解
删除匹配模式的字符在早期 C++ 版本中并无现成函数，程序员需要通过迭代查找和手动删除实现。随着 C++ 的发展，`std::erase` 和 `std::remove` 函数的应用为程序员提供了更方便的工具。

注意到上述代码中，`std::remove` 函数并不真正删除字符，而是将不需删除的字符向数组或字符串的前部移动，返回一个指向尾部的迭代器。然后，你可以使用 `std::erase` 来真正删除字符。

若要删除的字符匹配模式较复杂，如单词或短语，你可能需要使用正则表达式，例如 `std::regex replace`。

## 另请参阅：
[1] [C++ Reference: string::erase](http://www.cplusplus.com/reference/string/string/erase/)
[2] [C++ Reference: remove](http://www.cplusplus.com/reference/algorithm/remove/)
[3] [C++ Reference: regex::replace](http://www.cplusplus.com/reference/regex/regex_replace)