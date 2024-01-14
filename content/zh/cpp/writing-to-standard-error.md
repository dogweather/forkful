---
title:                "C++: “写入标准错误”"
simple_title:         "“写入标准错误”"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

为什么: 
在编写C++程序时，有时我们会遇到一些错误，这时通过把错误信息输出到标准错误流（standard error）中，可以帮助我们快速定位和修复错误。因此，了解如何写入标准错误流是一项非常重要的程序开发技能。

怎么做: 
可以通过在代码中加入```C++...```的代码块来演示如何写入标准错误流。首先，我们需要包含<fstream> 头文件，并使用std::cerr来表示标准错误流。然后，使用<<操作符来将需要输出的错误信息写入到标准错误流中，例如：
```C++
#include <fstream>

int main() {
    std::cerr << "这是一个错误信息" << std::endl;
    return 0;
}
```
运行这段代码，就会在控制台输出相应的错误信息。

深入了解： 
标准错误流和标准输出流（standard output）是两个不同的输出流，它们分别用于输出错误信息和普通信息。和标准输出流一样，我们也可以使用`std::cerr`来表示标准错误流。但是，与标准输出流不同的是，标准错误流在输出时不会被缓冲，即时输出，这样可以确保错误信息能够及时显示在控制台上，方便我们调试程序。

此外，我们也可以使用`std::clog`来表示标准错误流，它和`std::cerr`的用法是一样的，只是它是有缓冲的输出流，在某些情况下也可以用来输出错误信息。

总的来说，掌握如何写入标准错误流可以帮助我们更好地调试程序，提高工作效率。

## 参考链接：
- [C++ 文件流教程](https://www.runoob.com/cplusplus/cpp-files-streams.html)
- [C++ 标准库文档](https://zh.cppreference.com/w/cpp/io/iostream)
- [我和C++标准错误流的故事](https://yq.aliyun.com/articles/64451)

## 参见: 
- [C++ 标准输入流：cin](https://www.cnblogs.com/905816937New/p/12252793.html)
- [C++ 格式化输出：cout](https://zhuanlan.zhihu.com/p/133012818)
- [Debugging for beginners](https://code.visualstudio.com/docs/cpp/cpp-debug)