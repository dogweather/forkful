---
title:                "将字符串转换为小写"
html_title:           "C++: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

#关于将字符串转换为小写

##什么是字符串转换为小写？为什么程序员要这么做？

将字符串转换为小写指的是将字符串中的所有字母变为小写形式。这样做的好处是可以统一字符串的格式，便于程序的处理和比较。比如输入的字符串可能有些字母是大写，有些是小写，这样会影响到程序的运行。通过将字符串转换为小写，可以避免这样的问题。

##怎么做？

示例代码和输出结果如下所示：

```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
  // 定义一个字符串
  string str = "HeLlO wOrLd";
  
  // 转换为小写
  transform(str.begin(), str.end(), str.begin(), ::tolower);
  
  // 输出结果
  cout << str << endl;
  
  return 0;
}
```

输出结果应为：hello world

##深入了解

1. 历史背景：在早期的计算机编程中，存储空间非常有限，为了节省空间，字符串中的字母常常都是小写形式。随着计算机性能的提升，人们更加注重代码的可读性和可维护性，因此转换字符串为小写的需求也逐渐增加。
2. 其他方法：除了使用```transform```函数，还可以使用```tolower```函数逐个转换字符为小写。但是，由于```tolower```函数返回的是整型，会导致代码可读性较差。
3. 实现细节：在转换字符串为小写时，程序会对字符串中的每个字符进行判断，如果是大写字母，则通过ASCII码偏移转换为小写字母。因此，转换为小写的时间复杂度为O(n)，其中n为字符串的长度。

##相关链接

- [C++ string to lower case](https://www.geeksforgeeks.org/cpp-toupper-function/)
- [ASCII table](https://www.ascii-code.com/)