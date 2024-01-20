---
title:                "使用正则表达式"
html_title:           "Arduino: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

正则表达式是一种强有力的文本处理工具，主要用于匹配和操作字符串。程序员使用它们因为它们可以以极其简洁的方式实现复杂的文本处理任务。

## 如何使用:

以下是一段简单的C++代码，演示如何使用正则表达式匹配文本。

```C++
#include<regex>
#include<iostream>

int main(){
    std::string s ("hello world");
    std::regex e ("\\b\\w+\\b");

    std::regex_iterator<std::string::iterator> rit ( s.begin(), s.end(), e );
    std::regex_iterator<std::string::iterator> rend;

    while (rit!=rend) {
        std::cout<<rit->str()<<std::endl;
        ++rit;
    }

    return 0;
}
```
这段代码会输出文本中的每个单词, 分别是 "hello" 和 "world".

## 深入解析:

正则表达式的概念最早由美国计算机科学家肯·汤普森在1968年创造，用于编写Unix操作系统的编辑器。正则表达式现在已被广泛应用于各种编程语言中，包括C++。

除了正则表达式，还有其他一些方法也可以进行字符串处理，例如字符串查找和替换函数，但是它们的功能远不如正则表达式强大。

在C++中，使用正则表达式需要包含 `<regex>` 头文件，它定义了基于NFA的ECMAScript兼容的正则表达式引擎。

## 更多信息:

- 正则表达式在线测试工具: [https://regex101.com/](https://regex101.com/)
- ECMAScript正则表达式规范: [https://www.ecma-international.org/ecma-262/5.1/#sec-15.10](https://www.ecma-international.org/ecma-262/5.1/#sec-15.10)