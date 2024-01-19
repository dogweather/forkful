---
title:                "搜索和替换文本"
html_title:           "Kotlin: 搜索和替换文本"
simple_title:         "搜索和替换文本"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
寻找和替换文本是筛选代码的通用方法，它可以定位特定的字符串并将其替换为另一个字符串。程序员这样做主要是为了批量修改代码或文本信息。

## 怎么做？
我们可以使用 C++ 中的 `std :: string :: find()` 和 `std :: string :: replace()` 函数在字符串中搜索和替换文本。以下是一些使用示例：

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Hello, world!";
    size_t found = str.find("world");
    if(found != std::string::npos)
        str.replace(found, 5, "C++");
    std::cout << str << std::endl;
    return 0;
}
```

运行上述代码，输出如下：

```bash
Hello, C++!
```

## 深度探索
寻找和替换文本的概念可以追溯到计算机编程的早期阶段，当时人们意识到了搜索和修改代码有效性和便捷性。C++ 提供了一个现代化的接口，使这个过程变得更高效，更易于理解。

作为替代方案，还可以使用 `std :: string :: substr()` 和 `std :: stringstream` 来实现文本搜索和替换，但这需要编写更多的代码并降低了代码的可读性。

关于搜索和替换文本的实现细节，`std::string::find()` 的工作原理是遍历字符串中的每个字符，然后比较目标字符串；`std::string::replace()` 则是覆盖指定位置的字符。它们都使用了数据结构和算法基本原理。

## 参考资料
要了解更多信息，请参阅以下链接：
1. C++ 文本搜索和替换的详尽讨论和实例: [http://www.cplusplus.com/reference/string/string/replace/](http://www.cplusplus.com/reference/string/string/replace/)
2. C++ 中关于 string 的详细教程：[https://www.geeksforgeeks.org/stdstring-class-in-c/](https://www.geeksforgeeks.org/stdstring-class-in-c/) 
3. C++ 文档中的 `std::string::find()` 和 `std::string::replace()`：[https://en.cppreference.com/w/cpp/string/basic_string/find](https://en.cppreference.com/w/cpp/string/basic_string/find), [https://en.cppreference.com/w/cpp/string/basic_string/replace](https://en.cppreference.com/w/cpp/string/basic_string/replace).