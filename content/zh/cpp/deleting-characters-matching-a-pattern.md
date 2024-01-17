---
title:                "删除匹配模式的字符"
html_title:           "C++: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么和为什么?

删除符合特定模式的字符是一种编程的常见任务，它允许程序员通过一次操作来删除指定模式的多个字符。这种操作节省了时间和精力，并且可以提高代码的可读性。程序员使用这种技术来删除无用的字符，或者为不同的输入数据格式提供通用的处理方法。

## 怎样做？

下面是一个简单的例子，展示如何使用C++编程语言在字符串中删除所有的数字字符。

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string str = "Hello123 World456";
    int i = 0;
    while (i < str.length()) {
        if (isdigit(str[i])) {
            str.erase(i, 1);
        }
        else {
            i++;
        }
    }
    cout << str; // Output: HelloWorld
    return 0;
}
```

## 深入了解

这种技术最早出现在正则表达式中，但现在已被许多编程语言广泛采用。另外，除了一次删除所有匹配的字符，程序员也可以使用循环和条件语句来逐个删除指定模式的字符。在C++中，使用erase()函数可以通过指定起始位置和长度来删除字符串中的字符。

## 参考资料

- [C++标准模板库(STL)函式之string：erase()](https://www.twblogs.net/a/5bd6a9fa2b7177781a0b5d3a)  
- [C++教程-更多有用的C++字符函数](https://www.runoob.com/cplusplus/cpp-string-functions.html)  
- [C++文档-C++中的字符串处理](https://www.cplusplus.com/articles/1UqpX9L8/)