---
title:                "将一个字符串首字母大写"
html_title:           "C++: 将一个字符串首字母大写"
simple_title:         "将一个字符串首字母大写"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##为什么: 字符串大写有什么用？

字符串的大小写是编程中非常重要的一部分，它可以用来标识不同的数据类型，也能够帮助我们更好地处理数据。当我们需要统一字符串的表现形式时，即使是大小写也可以起到很大的作用。因此，掌握如何大写字符串是非常有用的技能。

##如何做：
首先，我们需要创建一个字符串变量，用来存储我们要大写的内容。假设我们要大写的字符串是"hello world"。然后，我们可以使用C++中的内置函数toupper()来将这个字符串转换为大写，具体的代码如下所示：
```C++
#include <iostream>
#include <string> // 必须包括string头文件

using namespace std;

int main()
{
  string str = "hello world"; // 创建一个字符串变量并赋值为"hello world"
  transform(str.begin(), str.end(), str.begin(), ::toupper); // 调用toupper函数进行转换
  cout << "大写字符串为：" << str << endl; // 输出转换后的字符串
  return 0;
}  
```
运行结果如下：
```
大写字符串为：HELLO WORLD
```

##深入探讨：
在上面的例子中，我们使用了C++中的内置函数toupper()来实现字符串大写的功能。但是，除了这个函数外，我们也可以利用C++中提供的其他方法来实现同样的效果。例如，我们可以使用循环遍历字符串的每个字符，并将其转换为大写形式。此外，字符串的大小写转换也可以应用在不同编码方式的文字中，比如中文。通过掌握不同的转换方法，我们可以更加灵活地应用在不同的情况下。

##看看这些：
- [C++ transform函数](https://www.cplusplus.com/reference/algorithm/transform/)
- [C++ 字符串遍历](https://www.geeksforgeeks.org/traverse-string-using-stdbegin-stdend-c/)
- [C++中文字符编码](https://wenKuang.github.io/blog/wp-content/uploads/2014/05/1072c89c5270341.png)

##参考链接:
此外，还有许多其他的资源可以帮助你更深入地了解字符串的大小写转换。对于想要进一步提升自己编程技能的读者，我们推荐阅读下面这些参考链接：
- [C++ Primer](https://www.oreilly.com/library/view/c-primer-fifth/9781306179937/)
- [C++参考手册](https://www.cplusplus.com/reference/)
- [牛客网](https://www.nowcoder.com/)

希望本篇文章能够帮助你掌握C++中字符串大写的基础知识，并为你未来的学习之路提供一些参考。祝你编程愉快！