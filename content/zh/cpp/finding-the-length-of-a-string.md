---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

字符串长度的查找是计算字符串中字符的数目，程序员这样做的原因是控制循环、比较字符串或者处理子字符串。

## 如何做：

下面是如何在C++中使用`length()`和`size()`函数来找到字符串长度的代码示例及样本输出。

```c++
#include <iostream>
#include <string>

int main() {
    std::string s = "Hello, World!";
    std::cout << "String: " << s << std::endl;
    std::cout << "Length using length(): " << s.length() << std::endl;
    std::cout << "Length using size(): " << s.size() << std::endl;
    return 0;
}
```

样本输出：
```
String: Hello, World!
Length using length(): 13
Length using size(): 13
```

## 深度剖析

在C++历史的早期版本，例如C++98，还无法使用 `.size()` 或 `.length()`. 程序员需要通过循环遍历字符串来手动计算长度。但在现代的C++，计算字符串长度变得非常简单。

至于`.length()`和`.size()`之间的选择，实际上没有太大差异，两者都返回字符串中的字符数量，只是来自不同的历史背景。

有些开发者可能喜欢用 `strlen()` 来计算字符串长度，这在语义上更清晰一些。但要注意 `strlen()` 只适用于C风格的字符串而不适用于C++风格的字符串。

## 还可查看

以下链接提供了查找字符串长度以及相关知识的更多信息：

1. C++ Reference - std::string::length: http://www.cplusplus.com/reference/string/string/length/
2. C++ Reference - std::string::size: http://www.cplusplus.com/reference/string/string/size/
3. C++ Reference - std::strlen: http://www.cplusplus.com/reference/cstring/strlen/