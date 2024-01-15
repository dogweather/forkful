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

## 为什么

为什么会有人想要去阅读文本文件呢？通常来说，读取文本文件是为了从中获取特定的信息，比如存储在文件中的数据或者文本内容。

## 怎么做

为了读取文本文件，我们需要借助C++中的文件输入流（ifstream）类。首先，我们需要包含`<fstream>`头文件，然后使用`ifstream`对象打开我们想要读取的文件，同时指定文件的打开模式。接着，我们可以利用输入运算符`>>`和相关的成员函数来从文件中读取数据。下面是一个简单的例子：

```C++
#include <iostream> 
#include <fstream> 
using namespace std; 

int main() 
{ 
  // 创建ifstream对象并打开文件 
  ifstream file("example.txt"); 
  
  // 判断文件是否成功打开 
  if (!file) 
  { 
    cout << "无法打开该文件！" << endl; 
    return 1; 
  } 
  
  // 从文件中读取内容并输出 
  char c; 
  while (file >> c) 
    cout << c; 
    
  // 关闭文件 
  file.close(); 
  return 0; 
} 
```

假如我们有一个名为`example.txt`的文本文件，内容为：

```
Hello world! 
```

运行以上代码，输出将会是：

```
Hello world! 
```

## 深入了解

除了简单的用法，我们还可以对文本文件进行更深入的处理。例如，我们可以使用文件流指针来指定读取文件的位置，或者利用`getline()`函数从文件中逐行读取内容。同时，我们也可以利用文件输出流（ofstream）类来创建并写入文本文件。如果想要了解更多关于文件的操作方法，可以参考相关的文档或教程。

## 查看更多

- [C++ Reference: ifstream类](https://en.cppreference.com/w/cpp/io/basic_ifstream)
- [C++文件输入/输出](https://www.cnblogs.com/graphics/archive/2010/07/24/1792977.html)
- [学习C++文件操作](https://blog.csdn.net/foruok/article/details/7272108)

## 参考资料

- [C++ Primer Plus（第6版）](https://book.douban.com/subject/27039335/)
- [C++ Primer（第5版）](https://book.douban.com/subject/25708312/)
- [C++标准库实践（第2版）](https://book.douban.com/subject/30265532/)