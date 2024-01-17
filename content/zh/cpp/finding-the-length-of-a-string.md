---
title:                "寻找字符串的长度"
html_title:           "C++: 寻找字符串的长度"
simple_title:         "寻找字符串的长度"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

什么以及为什么？

在编程中，一个常见的任务是要找出字符串的长度（即包含多少个字符）。程序员经常需要这样做，因为字符串是一种重要的数据类型，它们被用来存储和操作文本信息。

如何：

以下是一个用C++编写的示例代码，演示如何找出字符串的长度。要运行此代码，请将它复制粘贴到一个C++编译器中，并输入您想要测试的字符串。

```C++
#include <iostream>
#include <string>
 
using namespace std;

int main() {
    string str; // declare a string variable
    cout << "Enter a string: ";
    cin >> str; // get input from user
    int length = str.length(); // calculate string length
    cout << "Length of the string is: " << length; // print output
    return 0;
}
```

以下是一个示例输出：

```
Enter a string: hello
Length of the string is: 5 
```

深入探讨：

历史背景：在早期的计算机系统中，字符串长度通常是固定的，并且预先定义为程序的一部分。但是，随着可变长度字符串的出现，例如C语言中的字符数组，需要通过计算来找出字符串的长度。

其他方法：除了使用length函数，还可以使用循环遍历字符串的每个字符，并计算出总数来找出字符串的长度。但是，这种方法比直接调用函数更耗时，因此不被推荐。

实现细节：C++中通常使用string类来表示字符串，该类具有length函数，可以返回字符串的长度。该函数内部使用了一个叫做size_type的特殊数据类型，用来表示字符串的大小。

参考链接：

- String Length in C/C++: <https://www.geeksforgeeks.org/length-string-c/>
- C++ String Length: <https://www.programiz.com/cpp-programming/string-length>