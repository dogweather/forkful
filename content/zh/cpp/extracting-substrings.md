---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么以及为什么？ (What & Why?)

在C++编程中，提取子字符串是指从一个已存在的字符串中获取某一部分。程序员通常需要这样做以便分析或修改特定部分的数据。

## 实现方式： (How to:)

在C++中, `std::string::substr` 函数用于提取子字符串。其使用格式如下：

```C++
string substr (size_t pos, size_t len) const;
```
其中，`pos` 是开始的位置，`len` 是子字符串的长度。

以下是一个例子：
```C++
#include<iostream>
using namespace std;

int main() {
    string str = "Hello, World!";
    string str2 = str.substr(0, 5);
    cout << "Extracted substring is: " << str2 << endl;
    return 0;
}
```
输出：
```
Extracted substring is: Hello
```
此例子中 `"Hello"` 就是从 `"Hello, World!"` 中提取出来的子字符串。

## 深入了解： (Deep Dive)

**1. 历史背景**

`substr` 函数属于 `<string>` 标准库。它是C++标准库中的一个关键部分并用于处理字符串相关的操作。

**2. 选择方案**

除了 `std::string::substr`, 还可以通过利用 `std::string::find`， `std::string::begin`, `std::string::end` 等等来提取子字符串。你的选择取决于具体情境和需求。

**3. 实现细节**

`substr` 函数基本原理是通过开始位置 `pos` 及长度 `len` 数据，从原始字符串复制指定长度的字符到新字符串中。

## 更多信息 (See Also)

想要了解更多有关C++ string类函数，请访问以下链接:

- [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
- [cppreference,com](https://en.cppreference.com/w/cpp/string/basic_string)