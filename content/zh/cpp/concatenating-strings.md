---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么和为什么？
连接字符串是将两个字符串结合在一起形成一个新的字符串。程序员需要这样做以方便地将不同的数据元素格式化并结合在一起，从而实现不同的编程需求。

## 如何做：
在C++中，我们可以用 '+' 运算符来连接（或拼接）字符串。请看下面的例子：

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string str1 = "Hello, ";
    string str2 = "World!";
    string str3;
    str3 = str1 + str2;
    cout << str3 << endl;
    
    return 0;
}
```
此代码的执行结果是：`Hello, World!`

## 深度剖析
连接字符串的概念在编程的早期就已经存在了，这使得从文本/数据文件中的不同部分提取和组合信息变得翌易。

除了 "+" 运算符，C++还有其他方式来连接字符串，例如 `std::string::append()`，`std::stringstream` 或者 `std::strcat()` 等函数。

虽然 "+" 是最直接的方式，但在处理大数据量时，可能会退化为非常低效。解决这个问题的一种方式是使用 `std::stringstream` 或者 C++ 的标准库函数 `std::strcat()`。

## 参考下列资源
1. [wstring_cplusplus.com](http://www.cplusplus.com/reference/string/string/)
2. [cplusplus.com_string::append](http://www.cplusplus.com/reference/string/string/append/)
3. [stackoverflow_cpp::string::append_vs_operator+](https://stackoverflow.com/questions/299279/c-string-append-vs-operator)