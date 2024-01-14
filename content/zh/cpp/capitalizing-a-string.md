---
title:                "C++: 把字符串大写化"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么：字符串大写化的重要性

在编程中，字符串大写化是一项非常常见的任务。它可以帮助我们统一格式，提高代码的可读性，并避免可能出现的错误。此外，一些系统要求输入的字符串必须为大写才能正确运行，在这种情况下，字符串大写化就扮演着重要的角色。

## 如何进行：C++编程范例和输出示例

```C++
#include <iostream>
using namespace std;

void capitalizeString(string& str) {
    for (int i = 0; i < str.length(); i++) {
        if (islower(str[i])) {
            str[i] = toupper(str[i]); //使用toupper函数将小写字母改为大写
        }
    }
}

int main() {
    string input;
    cout << "请输入一个字符串：";
    getline(cin, input); //使用getline函数读取字符串，以确保可以读取包含空格的字符串
    capitalizeString(input); //调用capitalizeString函数对输入的字符串进行大写化
    cout << "大写化后的字符串为：" << input;
    return 0;
}
```

**输入：** Hello, world!

**输出：** 大写化后的字符串为：HELLO, WORLD! 

## 深入学习：关于字符串大写化的更多信息

在C++中，我们可以使用toupper函数将字符转换为大写。此外，还有一个tolower函数可以将字符转换为小写。我们可以使用循环结合这两个函数，对字符串中的每个字符进行检查并进行转换。同时，也可以使用string类中的transform函数来实现字符串大写化的功能。

另外，需要注意的是，对于包含非英文字母的字符串，如中文，toupper函数可能不起作用，因此需要对程序进行相应的修改，例如使用boost库中的函数来实现中文字符串的大写化。

## 参考资料

1. [toupper函数的用法](https://www.w3cschool.cn/cpp/cpp-func-toupper.html)
2. [string类中的transform函数](http://www.cplusplus.com/reference/string/string/transform/)
3. [boost库中文档的转换函数](https://www.boost.org/doc/libs/1_54_0/libs/locale/doc/html/conversions.html)

## 相关链接

- [如何用C++实现字符串反转](https://www.howtogeek.com/howto/programming/c-using-templates-to-reverse-a-string/)
- [C++标准库中的字符串操作函数](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)