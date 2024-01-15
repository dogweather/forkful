---
title:                "计算字符串的长度"
html_title:           "C++: 计算字符串的长度"
simple_title:         "计算字符串的长度"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

该文主要介绍如何在C++中找到一个字符串的长度，并且深入介绍了为什么需要这样做。通过多个代码示例和输出结果来展示具体的操作方法，帮助读者更好地掌握这一技巧。

## 为什么

在编写程序时，我们经常需要对字符串进行操作，而了解一个字符串的长度是很重要的一部分。通过找到一个字符串的长度，我们可以在程序中更方便地处理字符串，从而提高程序的效率和可读性。

## 如何做

我们可以使用C++提供的标准库函数"```size()```"来获取一个字符串的长度。下面是一个简单的示例：

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "Hello World";
    cout << "字符串的长度为：" << str.size() << endl;
    return 0;
}
```

运行以上代码，将会输出："字符串的长度为：11"。这里，我们首先包含了"```string```"头文件，然后声明了一个字符串变量"```str```"，并将字符串赋值为"```Hello World```"。接着使用"```size()```"函数来获取字符串的长度，并将其输出到控制台。

除了"```size()```"函数，我们也可以使用另一个标准库函数"```length()```"来获取字符串的长度，它的用法和"```size()```"完全一样。同时，我们也可以通过遍历字符串来计算其长度，具体代码如下：

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "Hello World";
    int length = 0;

    for (int i = 0; str[i] != '\0'; i++) {
        length++;
    }

    cout << "字符串的长度为：" << length << endl;
    return 0;
}
```

在这段代码中，我们使用了一个for循环来遍历字符串，每次判断当前字符是否为"```'\0'```"，即字符串的结尾符，如果不是，则将长度加一。最后输出的结果与"```size()```"函数获取的长度相同。

## 深入探讨

在C++中，字符串的长度其实是指字符串中字符的个数，而不是字符串占用的内存大小。这是因为C++中的字符串其实是由一系列字符构成的，每个字符占用一个字节的内存空间。所以，当我们使用"```sizeof()```"函数来获取字符串的长度时，得到的结果为字符串占用的内存大小，而不是字符串中字符的个数。这点需要注意。

此外，我们还可以通过使用"```strlen()```"函数来获取字符串的长度，它属于C语言中的函数，但也可以在C++中使用。"```strlen()```"函数的用法和"```size()```"和"```length()```"完全一样，都是返回字符串中字符的个数。

## 参考资料

- [C++ Reference - String size()](https://www.cplusplus.com/reference/string/string/size/)
- [C++ Reference - String length()](https://www.cplusplus.com/reference/string/string/length/)
- [C++: sizeof char Array, sizeof std::string](https://stackoverflow.com/questions/262320dfsdfsdf3c-cout-strlen-strlen-strlen-ds)
- [C++ Reference - String strlen()](https://www.cplusplus.com/reference/clibrary/cstring/strlen/)

## 参见

- [C++ 字符串基础教程](https://www.geeksforgeeks.org/c-strings/#Introduction)
- [学习如何在 C++ 中处理字符串](https://www.programiz.com/cpp-programming/string-handling)