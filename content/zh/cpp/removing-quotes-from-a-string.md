---
title:                "从字符串中移除引号"
aliases:
- zh/cpp/removing-quotes-from-a-string.md
date:                  2024-01-26T03:38:11.130876-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中移除引号"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/removing-quotes-from-a-string.md"
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
