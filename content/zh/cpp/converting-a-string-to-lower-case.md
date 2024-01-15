---
title:                "转换字符串为小写"
html_title:           "C++: 转换字符串为小写"
simple_title:         "转换字符串为小写"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么
为什么要把字符串转换为小写？在编程中，有时我们需要对字符串进行一些比较，而不考虑大小写。因此，将字符串转换为小写可以方便地进行这样的比较。

## 如何
我们可以使用C++中的内置函数 `tolower` 来将字符串转换为小写。下面是一个示例代码，将输入的字符串转换为小写，并通过 `cout` 输出结果。

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string input;
    cout << "请输入一个字符串：";
    cin >> input;

    for (int i = 0; i < input.length(); i++) {
        input[i] = tolower(input[i]);
    }

    cout << "转换后的字符串为：" << input << endl;

    return 0;
}
```

输入：
```
HELLO WORLD
```

输出：
```
转换后的字符串为：hello world
```

## 深入探讨
其实，转换字符串为小写的过程并不复杂。 `tolower` 函数通过改变每个字符的 ASCII 值来实现大小写转换。数字和符号不受影响，只有字母会被转换为小写形式。

## 参考链接
- [C++ 字符串转换大小写](https://www.runoob.com/w3cnote/cpp-string-tolower.html)
- [C++ tolower函数](https://cplusplus.com/reference/cctype/tolower/)
- [C++ 字符串基本操作](https://www.cnblogs.com/scrcode/p/4688483.html)

## 参见
- [C++中的字符串操作](https://www.notion.so/8fa4b3c21d7446398804f0acee397f4d)
- [C++中的数据类型](https://www.notion.so/dff57f60bfa648b5b2652a60d0fa4f77)