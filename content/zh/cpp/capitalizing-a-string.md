---
title:                "字符串大写化"
html_title:           "C++: 字符串大写化"
simple_title:         "字符串大写化"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 什么是大写字符串？为什么程序员这么做？

大写字符串是指将字符串中的所有字符都转换成大写形式。程序员们经常这么做是因为它可以方便比较字符串，更准确地进行判断逻辑，以及避免大小写导致的错误。

# 如何实现大写字符串？

下面是一个使用C++语言的示例代码，以及它的输出结果：

```C++
#include <iostream>
#include <cstring>
using namespace std;

int main() {
  // 定义一个字符串
  string str = "hello, WORLD";
  // 使用C++中的内置函数toupper()将字符串转换为大写形式
  for (int i = 0; i < str.length(); ++i) {
    str[i] = toupper(str[i]);
  }
  // 输出结果为"HELLO, WORLD"
  cout << str << endl;
  return 0;
}
```

# 深入了解大写字符串

## 历史背景

在早期的计算机系统中，字符和字符串都是以ASCII码来表示的，其中大写和小写的字母是有着固定的对应关系的。随着计算机技术的发展，Unicode编码开始被广泛使用，大小写的概念变得更加复杂，因此在比较字符串时，将所有字符都统一为大写形式可以避免一些不必要的问题。

## 其他方法

除了使用C++中的内置函数toupper()，程序员们也可以使用不同的方法来实现大写字符串，比如使用第三方库、自定义函数等。

## 实现细节

在C++中，字符和字符串都是以数组的形式保存在内存中的，因此要将所有字符都转换为大写形式，只需要遍历这个数组并对每个字符进行转换即可。同时，要注意处理特殊字符和非字母字符的情况。

# 参考资料

- [C++参考手册](http://www.cplusplus.com/reference/)
- [ASCII编码表](https://www.asciitable.com/)
- [Unicode编码表](https://unicode-table.com/cn/)
- [C++中的字符串处理函数](https://www.geeksforgeeks.org/string-class-in-c/)