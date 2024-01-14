---
title:                "C++: 从计算机编程讲解：提取子字符串"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么？

字符串是C++中经常用到的数据类型，它们由多个字符组成并占用内存空间。有时候，我们需要从一个字符串中提取出一部分字符，这就是提取子字符串的原因。比如说，我们可能需要从一个包含用户的姓名的字符串中提取出姓氏，或者从一个URL中提取出网站的域名。

## 如何

提取子字符串的方法非常简单，只需要使用一个内置函数`substr()`即可。这个函数接受两个参数，分别是子字符串的起始位置和要提取的字符数。让我们来看一个简单的例子：

```C++
#include <iostream>
using namespace std;

int main() {
    string str = "Hello World!";
    string substr = str.substr(6, 5);
    cout << substr << endl;
}
```

在这个例子中，我们首先定义了一个字符串`str`，然后通过调用`substr()`函数提取出索引为6到索引为10（不包括10）的5个字符，也就是`World`。最后，我们将提取出的子字符串打印出来，输出结果为`World`。

值得注意的是，`substr()`函数的第一个参数是起始位置，而不是索引。如果我们想要从字符串的开头开始提取，可以传入0作为第一个参数。

## 深入了解

了解字符串的索引和子字符串的起始位置之间的关系是非常重要的。C++中，字符串的第一个字符的索引是0，倒数第一个字符的索引是字符串的长度减1。因此，如果我们想要从字符串的最后一个字符开始提取子字符串，我们可以使用如下代码：

```C++
#include <iostream>
using namespace std;

int main() {
    string str = "Hello World!";
    string substr = str.substr(str.size() - 5, 5);
    cout << substr << endl;
}
```

这个例子中，我们先使用`size()`函数获取字符串的长度，然后再将其减去需要提取的字符数，这样就得到了子字符串的起始位置。最终输出的结果为`World`。

除了使用`substr()`函数，C++中还有其他一些方法可以提取子字符串，比如使用`copy()`函数或者手动遍历字符串并保存需要提取的字符。深入了解字符串的操作和相关函数可以让我们更加灵活地处理字符串。

## 参考资料

了解更多关于提取子字符串的方法和技巧，请参考以下资料：

- [C++ String substr()](http://www.cplusplus.com/reference/string/string/substr/)
- [C++ String Operations](https://www.geeksforgeeks.org/cpp-string-class-and-its-applications/)
- [C++ Strings](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)

## 参见

- [C++ String Functions](https://www.tutorialspoint.com/cplusplus/cpp_string_functions.htm)
- [C++ String Manipulation](https://www.javatpoint.com/cpp-string-manipulation)