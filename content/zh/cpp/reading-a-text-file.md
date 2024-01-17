---
title:                "读取文本文件"
html_title:           "C++: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 什么&为什么？
将文本文件读取是指从计算机中读取文本文件并将其内容存储在程序中，以供代码进行处理。程序员经常要读取文本文件，以便获得数据或配置信息，从而使程序更加灵活和可配置。

# 如何：
下面是一个简单的示例，展示如何在C++中读取文本文件：
```
#include <iostream>
#include <fstream>

using namespace std;

int main(){
  ifstream file("input.txt"); //打开文本文件
  while(!file.eof()){ //循环直到文件末尾
    string line; //定义一个字符串变量用于存储每行内容
    getline(file, line); //读取一行内容，并存储在line中
    cout << line << endl; //打印出每行内容
  }
  file.close(); //关闭文件
}
```
如果我们有一个名为“input.txt”的文本文件，其中包含以下内容：
```
这是第一行文本。
这是第二行文本。
这是最后一行文本。
```
运行上面的示例代码，将会在控制台输出以下内容：
```
这是第一行文本。
这是第二行文本。
这是最后一行文本。
```

# 深入探究：
文本文件读取在计算机编程历史中起着重要作用。在过去，程序员通常使用低级编程语言来读取文本文件，如C语言中的`fopen()`和`fread()`函数。随着高级编程语言的发展，如C++，读取文本文件变得更加简单和直观。

除了使用C++的`ifstream`类读取文本文件外，还有其他方法，如使用标准库中的`std::getline()`函数。此外，还有第三方库可用于更复杂的文本文件读取，如Boost库中的`boost::iostreams`。

在实现文本文件读取时，需要考虑文本编码和换行符的问题。不同的文本文件使用不同的编码方式，因此需要选择正确的编码方式来读取文件。同时，部分操作系统使用不同的换行符，如Windows使用回车换行（CRLF），而类Unix系统使用换行符（LF），因此需要注意。

# 请参阅：
- [C++ ifstream类](https://www.geeksforgeeks.org/ifstream-class-in-c-stl/)
- [标准库getline()函数](https://www.cplusplus.com/reference/string/string/getline/)
- [Boost库iostreams模块](https://www.boost.org/doc/libs/1_77_0/libs/iostreams/doc/index.html)