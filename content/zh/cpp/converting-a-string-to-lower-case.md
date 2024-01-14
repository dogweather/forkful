---
title:    "C++: 将字符串转换为小写"
keywords: ["C++"]
---

{{< edit_this_page >}}

为什么：虽然大小写对于编程并不是关键问题，但有时我们需要将一个字符串转换为小写字母。这在比较字符串时可能会有帮助，因为它忽略了大小写的差异。

如何操作：```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
  // 定义一个字符串变量
  string text = "Hello World";

  // 使用循环将每个字符转换为小写字母
  for (int i = 0; i < text.length(); i++) {
    text[i] = tolower(text[i]);
  }

  // 输出转换后的字符串
  cout << text << endl;

  return 0;
}
```

输出：

```
hello world
```

深入探讨：字符串类型的变量在C++中是以字符数组的形式存储的，因此我们可以通过遍历数组，将字符的ASCII码相应地加上32来实现小写转换。另外，使用C++标准库中的```tolower()```函数也可以方便地将字符转换为小写。这个函数在```<cctype>```头文件中定义。

另外，如果需要将字符串转换为大写字母，我们可以使用```toupper()```函数来实现类似的操作。

参见：[C++ 字符串转换为小写](https://www.programiz.com/cpp-programming/library-function/cctype/tolower)、[C++ 字符串转换为大写](https://www.programiz.com/cpp-programming/library-function/cctype/toupper)

## 进一步阅读

- [C++字符串：字符、字符串和字符串函数](https://www.programiz.com/cpp-programming/string)
- [C++程序语言：字符串操作](https://zh.cppreference.com/w/cpp/string/basic_string)