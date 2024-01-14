---
title:    "C++: 提取子字符串"
keywords: ["C++"]
---

{{< edit_this_page >}}

## 为什么要提取子字符串

C++是一种广泛使用的编程语言，它允许用户操作字符串。提取子字符串是一种常见的字符串处理操作，它能够让我们从一个较长的字符串中提取出我们需要的部分。这在编程中非常有用，因为它可以帮助我们简化代码，提高程序的执行效率。

## 如何提取子字符串

提取子字符串可以通过string类中的substr函数来实现。以下是一个简单的示例代码：

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
  string str = "Hello World";
  string sub_str = str.substr(6,5); //从第6个字符开始提取5个字符
  cout << sub_str; //输出"World"
  return 0;
}
```

输出结果为“World”，我们可以看到通过substr函数成功地提取出了“World”这个子字符串。

## 深入了解提取子字符串

除了指定起始位置和长度，还可以通过substr函数的另一个版本来提取子字符串。该版本的函数原型为：

```C++
string substr(size_t pos = 0, size_t len = npos) const;
```

其中，pos代表起始位置，len代表要提取的长度。若不指定len，则默认为字符串结尾。另外，可以使用npos来代表字符串结尾，这在处理不确定长度的字符串时非常方便。

## 参考文章

- [C++ string::substr 函数](https://www.cplusplus.com/reference/string/string/substr/)
- [C++ string 类](https://www.runoob.com/cplusplus/cpp-strings.html)

## 参见

- [字符串处理的其他操作](https://www.runoob.com/cpp/cpp-string.html)
- [C++中字符串的基本操作](https://www.geeksforgeeks.org/cpp-string-class-and-its-applications/)