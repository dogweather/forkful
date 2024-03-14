---
date: 2024-01-26 03:38:11.130876-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u5220\u9664\u5F15\u53F7\u610F\u5473\u7740\
  \u53BB\u9664\u5305\u56F4\u6211\u4EEC\u6587\u672C\u7684\u90A3\u4E9B\u70E6\u4EBA\u7684\
  \u53CC\u5F15\u53F7\u6216\u5355\u5F15\u53F7\uFF08' \u6216 \"\uFF09\u3002\u7A0B\u5E8F\
  \u5458\u7ECF\u5E38\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5BF9\u8F93\u5165\u8FDB\u884C\
  \u6D88\u6BD2\uFF0C\u5C06\u6587\u672C\u5B58\u50A8\u5728\u6570\u636E\u5E93\u4E2D\uFF0C\
  \u6216\u4E3A\u6CA1\u6709\u5F15\u53F7\u6742\u8D28\u7684\u5B57\u7B26\u4E32\u8FDB\u4E00\
  \u6B65\u5904\u7406\u505A\u51C6\u5907\u3002"
lastmod: '2024-03-13T22:44:48.097072-06:00'
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u5220\u9664\u5F15\u53F7\u610F\u5473\u7740\
  \u53BB\u9664\u5305\u56F4\u6211\u4EEC\u6587\u672C\u7684\u90A3\u4E9B\u70E6\u4EBA\u7684\
  \u53CC\u5F15\u53F7\u6216\u5355\u5F15\u53F7\uFF08' \u6216 \"\uFF09\u3002\u7A0B\u5E8F\
  \u5458\u7ECF\u5E38\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5BF9\u8F93\u5165\u8FDB\u884C\
  \u6D88\u6BD2\uFF0C\u5C06\u6587\u672C\u5B58\u50A8\u5728\u6570\u636E\u5E93\u4E2D\uFF0C\
  \u6216\u4E3A\u6CA1\u6709\u5F15\u53F7\u6742\u8D28\u7684\u5B57\u7B26\u4E32\u8FDB\u4E00\
  \u6B65\u5904\u7406\u505A\u51C6\u5907\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
---

{{< edit_this_page >}}

## 什么 & 为什么?
从字符串中删除引号意味着去除包围我们文本的那些烦人的双引号或单引号（' 或 "）。程序员经常这样做是为了对输入进行消毒，将文本存储在数据库中，或为没有引号杂质的字符串进一步处理做准备。

## 如何操作:
这是一个在C++中除掉引号的简单方法：

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hello, 'World'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

运行这个，你会得到：

```
Hello, World!
```

瞧！引号消失了。

## 深入探究
自计算机黎明时期以来，引号一直是文本上的一个麻烦。在过去，你会看到程序员务必循环遍历每个字符来过滤掉那些引号。今天，我们有了标准模板库（STL）中的 `std::remove` 来做这个繁重的工作。

有替代方法吗？当然！你可以使用 `std::regex` 的正则表达式来针对引号，但这有点像用大锤子砸核桃——强大，但对于简单任务可能是过度杀伤的。对于那些偏好最近C++版本的人，你可能会尝试使用 `std::string_view` 进行非修改性方法。

在实施上，要记住， `std::remove` 实际上并不会从容器中删除元素；它将未被删除的元素向前移动，并返回一个迭代器，这个迭代器位于新范围的末尾之后。这就是为什么我们需要使用 `erase` 方法来切掉不想要的尾巴。

## 参见
- C++ `std::remove` 参考： [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- 更多关于 `std::string` 操作： [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
