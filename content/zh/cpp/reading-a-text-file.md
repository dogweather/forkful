---
title:                "读取文本文件"
html_title:           "Kotlin: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么和为什么？

读取文本文件就是将文件中的数据传输到程序中。程序员这样做是为了能够处理并用这些数据完成某些任务。

## 如何操作：

以下的代码示例展示了如何在C++中读取文本文件：
```C++
#include <iostream>
#include <fstream>
#include <string>
using namespace std;

int main () {
  string line;
  ifstream myfile ("example.txt"); //打开文件
  if (myfile.is_open()){
    while ( getline (myfile,line) ){   //逐行读取文件内容
      cout << line << '\n';            //输出每行内容
    }
    myfile.close();                    //关闭文件
  }
  else cout << "Unable to open file"; 
  return 0;
}
```
输出示例：
```
This is a line.
This is another line.
```
以上代码从 "example.txt" 文件中读取内容，并一行行地显示它。

## 深入了解：

读取文本文件是计算机程序的基础之一，因为我们往往需要从文本文件中获取数据。文本文件的优点是它们可以简单地由人类创建和读取，且通用性广泛。

但也有其他方式可以读取数据，例如从数据库或通过网络接口。它们的使用取决于你的特定需求。

在C++中，我们通常使用 fstream库 编写和读取文件。其中，ifstream 类用于从文件读取数据，而 ofstream 类用于向文件写入数据。

## 参考资料：

以下是一些相关的学习资源：
1. [Cplusplus.com: Basic file operations](http://www.cplusplus.com/doc/tutorial/files/)
2. [GeeksforGeeks: C++ File Handling](https://www.geeksforgeeks.org/file-handling-c-classes/)