---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？
字符串插值是一种通过在字符串中插入变量值来构建新字符串的方法。程序员使用这种方法可以使代码更清晰易读，更易于维护。

## 如何操作：
在C++中，我们可以使用sstream类从标准库中实现字符串插值，下面是一个简单的例子:

```C++
#include <iostream>
#include <sstream>

int main() {
    std::string name = "John";
    int age = 30;
    std::stringstream ss;
    ss << "My name is " << name << " and I am " << age << " years old.";
    std::string sentence = ss.str();
    std::cout << sentence << std::endl;
    return 0;
}

```
运行这段代码，将输出：

```C++
My name is John and I am 30 years old.
```
## 深入探讨：
#### 历史背景
字符串插值首次出现在早期的编程语言中，例如Perl和Unix shell脚本。C++原生并不支持字符串插值，但提供了可以达到相似效果的处理方式。

#### 可选方案
在C++20中，添加了格式库，提供了更现代，更具可读性的替代方案实现字符串插值。如下面的例子：

```C++
#include <format>
...
std::string sentence = std::format("My name is {} and I'm {} years old.", name, age);

```
#### 实现细节
在C++中，`std::stringstream`对象用于读取和写入字符串。使用`<<`操作符向ss对象添加内容，而使用str()函数获取其内容。

## 参考资料：
- `std::stringstream` 类的文档: [点击这里](https://en.cppreference.com/w/cpp/io/basic_stringstream)
- C++20 `std::format` 方法的文档: [点击这里](https://en.cppreference.com/w/cpp/utility/format/format)
- 维基百科上关于字符串插值的内容: [点击这里](https://en.wikipedia.org/wiki/String_interpolation)