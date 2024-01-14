---
title:                "C++: 将字符串转换为小写 (Jiāng zìfú chuànghuàn wéi xiǎoxiě)"
simple_title:         "将字符串转换为小写 (Jiāng zìfú chuànghuàn wéi xiǎoxiě)"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，字符串是非常常见的一种数据类型。有时候，我们需要将字符串中的所有字符都转换成小写字符。这可能用于数据清洗，比如将用户输入的字符串统一为小写，或者用于字符串比较时的大小写不敏感的情况。下面我们将学习如何在C++中实现将字符串转换为小写字符。

## 如何做

完整代码和输出见下方代码块：

```C++
#include <iostream>
#include <string>

using namespace std;

// 定义一个函数，接收string类型参数，并返回转换为小写的字符串
string toLowerCase(string str) {
    // 遍历字符串中的每个字符
    for (int i = 0; i < str.length(); i++) {
        // 调用tolower函数将字符转换为小写，再通过引用赋值给原来的字符
        str[i] = tolower(str[i]);
    }
    return str;
}

int main() {
    // 输入一段字符串
    string input;
    cout << "请输入一段字符：" << endl;
    cin >> input;
    // 调用函数将字符串转换为小写
    string result = toLowerCase(input);
    // 输出转换后的结果
    cout << "转换后的字符串为：" << result << endl;
    return 0;
}
```

输入：

```
Programming BLOG for Mandarin Readers
```

输出：

```
转换后的字符串为：programming blog for mandarin readers
```

## 深入探讨

在C++中，字符串是以字符数组的形式存储的。因此我们可以通过遍历字符串中的每个字符来实现转换为小写的操作。在上面的代码中，我们使用了标准库中的`string`和`tolower`函数来实现转换。另外，我们还可以使用更底层的方法如ASCII码来进行字符转换。

## 参考链接

- [C++ string类的字符串处理函数](https://www.cnblogs.com/chenhanzhun/archive/2013/04/16/3028441.html)
- [C++的控制结构——循环结构FOR、WHILE和DO WHILE循环](https://www.cnblogs.com/allensun/archive/2011/10/06/2200622.html)
- [如何在C++中将字符串转换为大写或小写](https://www.programiz.com/cpp-programming/library-function/cctype/tolower)  

## 参见

- [文本处理技巧：将字符串转换为小写](https://www.jianshu.com/p/4d8d88176e21)
- [C++ Primer 中文版（第五版）](https://book.douban.com/subject/25708312/)