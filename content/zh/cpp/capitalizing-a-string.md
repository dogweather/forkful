---
title:                "C++: 字符串的大写化"
simple_title:         "字符串的大写化"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 为什么

在C++编程中，有时候我们需要将一个字符串中的每个单词的第一个字母大写，例如将"hello world"变成"Hello World"。这在一些应用中是十分常见的需求，例如处理用户输入或者格式化输出。因此，了解如何实现字符串的首字母大写功能是很有用的。

# 如何实现

首先，我们需要引入C++中内置的string库，它提供了一些与字符串相关的方法和函数。然后，我们可以使用for循环逐个遍历字符串中的字符，并利用toupper()函数将小写字母转换为大写字母。最后，我们可以打印出转换后的字符串或者将其赋值给一个新的变量。

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "hello world";

    // Loop through each character in the string
    for (int i = 0; i < str.length(); i++) {
        // Convert lowercase letters to uppercase
        str[i] = toupper(str[i]);
    }

    // Print the converted string
    cout << str << endl;

    // Output: HELLO WORLD
    
    return 0;
}
```

# 深入了解

除了toupper()函数，我们还可以使用C++中提供的其他方法来实现字符串的首字母大写功能。例如，我们可以使用transform()函数结合toupper()函数来实现字符串的首字母大写。同时，我们还可以利用字符串流类stringstream来处理更复杂的字符串操作。

# 参考资料

- [C++字符串的转换和大小写转换](https://blog.csdn.net/SailFor/article/details/20667723)
- [C++ string类介绍](https://www.runoob.com/cplusplus/cpp-class-string.html)
- [C++ stringstream类详解](https://blog.csdn.net/chenhanzhun/article/details/4238146)

# 参见

- [C++中的字符串处理](https://www.runoob.com/cplusplus/cpp-string-handling.html)
- [C++中的字符转换](https://www.tutorialspoint.com/cplusplus/cpp_char_conversion.htm)
- [C++中的流处理](https://www.geeksforgeeks.org/c-string-class-and-its-applications/)