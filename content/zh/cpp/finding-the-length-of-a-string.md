---
title:                "C++: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么

在编写C++程序时，我们经常需要对字符串进行操作，包括获取字符串的长度。通过获取字符串的长度，我们可以更好地掌握字符串的属性，并根据需要进行进一步的处理。因此，了解如何找到字符串的长度是非常重要的。在本文中，我将向大家介绍如何使用C++语言来找到字符串的长度，并深入探讨一些相关的知识点。

# 如何找到字符串的长度

要找到字符串的长度，我们可以使用C++中的内置函数`length()`。该函数的作用是返回所输入字符串的长度，单位为字符数。让我们来看一个简单的例子：

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "你好，世界！";
    int len = str.length();

    cout << "字符串的长度为：" << len << "个字符。" << endl;

    return 0;
}
```

输出结果为：

```
字符串的长度为：6个字符。
```

上面的例子中，我们首先声明了一个字符串变量`str`，并将一个字符串赋值给它。然后通过调用`length()`函数获取字符串的长度，并将结果赋值给整型变量`len`。最后，我们使用`cout`语句将结果输出到屏幕上。

除了使用`length()`函数，我们也可以使用C++中的另一个内置函数`size()`来获取字符串的长度。两者的作用都相同，只是名称不同而已。

# 深入探讨

在C++中，字符串的长度是以`int`类型来表示的，因此字符串的最大长度也受到`int`类型的限制。如果字符串的长度超过了`int`类型的最大值，那么就无法正确地获取字符串的长度。并且，在C++中，字符串的长度不包括末尾的空字符`\0`。也就是说，如果一个字符串的长度为5，那么它实际上包含6个字符（包括最后的空字符）。

此外，对于Unicode编码的字符串，使用`length()`函数获取的长度可能并不准确。因为Unicode编码中的一些字符可能占用2个或更多个字节，与ASCII编码中的每个字符占用一个字节不同。因此，要正确地获取Unicode字符串的长度，我们可以使用`wchar_t`类型来声明字符串变量，并调用`wstring`类中的`length()`函数来获取长度。

# 参考资料

- [C++中获取字符串长度的几种方法](https://zhuanlan.zhihu.com/p/79496333)
- [C++中获取字符串长度的各种方法](https://www.jianshu.com/p/e4b0c47a011b)
- [C++字符串：length()和size()的区别](https://blog.csdn.net/guotianqing/article/details/52689917)

# 参见

- [C++ string类](https://www.runoob.com/cplusplus/cpp-string.html)
- [C++基础：string类的使用](https://www.cnblogs.com/liangliangx/p/4599555.html)