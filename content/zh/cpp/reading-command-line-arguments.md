---
title:                "C++: 读取命令行参数"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

命令行参数是C++程序开发中一个重要的概念。通过读取命令行参数，我们可以为程序提供更多的灵活性和可定制性。这篇博文将带领大家深入了解如何在C++中读取命令行参数，并给出一些实用的代码示例。

## 如何做

在C++中，使用`argc`和`argv`这两个参数来读取命令行参数。其中，`argc`代表命令行参数的数量，而`argv`则是一个包含所有参数值的字符串数组。下面是一个示例代码，演示如何通过循环来读取所有命令行参数，并将它们打印出来：

```C++
#include <iostream>

int main(int argc, const char * argv[]) {
    // 循环读取每个命令行参数
    for (int i = 0; i < argc; i++) {
        // 打印命令行参数的值
        std::cout << argv[i] << std::endl;
    }
    
    return 0;
}
```

假设我们将上述代码保存为`read_args.cpp`，并在命令行中执行以下命令：

```
./read_args foo bar baz
```

那么程序输出的结果将是：

```
./read_args
foo
bar
baz
```

## 深入了解

除了上述基本的读取方法，C++还提供了一些更具灵活性的方式来读取命令行参数。比如，我们可以使用第三方库如`getopt`来实现更复杂的命令行参数解析。同时，也可以通过使用`std::stringstream`来将命令行参数转换为不同的数据类型，如`int`或`double`等。

另外，还可以通过`argc`和`argv`来实现一些简单的命令行参数处理，如命令行选项和帮助文档等。

总而言之，掌握命令行参数的读取方法，可以为我们的C++程序带来更多的灵活性和可定制性，同时也可以让我们的程序更加友好和易用。

## 参考链接

- [C++ 命令行参数教程](https://www.runoob.com/w3cnote/cpp-cmd-argc-argv.html)
- [使用 getopt 读取命令行参数](https://www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html)
- [C++ stringstream 教程](http://www.cplusplus.com/reference/sstream/stringstream/)

## 参见

- [C++ 基础教程](https://www.runoob.com/cplusplus/cpp-tutorial.html)
- [C++ 标准库参考手册](http://www.cplusplus.com/reference/)