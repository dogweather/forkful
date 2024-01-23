---
title:                "编写文本文件"
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)

在C++中写入文本文件是存储数据的一种方式。程序员这么做是为了保存信息，以便将来使用或记录日志。

## How to: (怎么做：)

写入文本文件用到`<fstream>`库。一个简单的C++示例是这样的：

```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
    ofstream myfile("example.txt");
    if (myfile.is_open()) {
        myfile << "Hello, World!\n";
        myfile.close();
        cout << "File written successfully";
    } else cout << "Unable to open file";
    return 0;
}
```
运行这段代码，你会得到一个名为`example.txt`的文本文件，内容是`Hello, World!`。

## Deep Dive (深入探索)

写入文本文件这个概念早在C语言中就有了，而C++则引入了流类来提高这一过程的抽象性和安全性。除了使用`<fstream>`，也可以用C风格的`FILE*`和`fprintf`，但这样做风险更高，因为需要手动管理文件的打开和关闭。现代C++鼓励使用RAII（资源获取即初始化）原则，使用`<fstream>`则自动帮助你处理这些问题。

## See Also (另请参阅)

- C++ 文件和流: [https://www.cplusplus.com/doc/tutorial/files/](https://www.cplusplus.com/doc/tutorial/files/)
- 经典C++教程《C++ Primer》相关章节
- C++ RAII原则解释: [https://en.cppreference.com/w/cpp/language/raii](https://en.cppreference.com/w/cpp/language/raii)
