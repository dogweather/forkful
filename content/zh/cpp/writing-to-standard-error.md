---
title:                "C++: “编写标准错误”"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

### 为什么

为什么要学习将错误信息写入标准错误流？

在编写程序中，我们经常会遇到各种错误。将错误信息写入标准错误流（stderr）可以帮助我们更好地调试和修复程序中存在的问题。学习如何写入标准错误流，可以让我们更加高效地处理程序中的错误，提高代码质量和可靠性。

### 如何做

下面我们就来看看如何在C++中将错误信息写入标准错误流。首先，我们需要包含 `<iostream>` 头文件，并使用 `cerr` 对象来表示标准错误流。

```C++
#include <iostream>

int main() {
  std::cerr << "这是一条错误信息" << std::endl;
  return 0;
}
```

代码中的 `std::endl` 会在输出信息后添加换行符，使得错误信息更加易读。当我们运行这段代码时，会在控制台中输出相应的错误信息。

### 深入了解

除了常规的输出错误信息，我们还可以使用 `std::exit()` 函数来结束程序的执行，并返回一个指定的错误码。在C++标准中，我们可以使用 `EXIT_FAILURE` 和 `EXIT_SUCCESS` 来表示不同的错误码。下面是一个实例：

```C++
#include <iostream>
#include <cstdlib>

int main() {
  std::cerr << "这是一条错误信息" << std::endl;
  std::exit(EXIT_FAILURE);
  std::cout << "这条语句将不会执行" << std::endl;
  return 0;
}
```

在实际编程中，我们可以结合条件语句以及其他的错误处理机制来有效地利用这种方式输出错误信息并结束程序的执行。

### 参考链接

- [C++错误处理与异常](https://www.runoob.com/cplusplus/cpp-exceptions-handling.html)
- [C++标准库文档-错误处理](https://zh.cppreference.com/w/cpp/error)
- [Cppreference-std::cerr](https://en.cppreference.com/w/cpp/io/cerr)
- [Cppreference-std::exit](https://en.cppreference.com/w/cpp/utility/program/exit)

### 相关阅读

以上只是错误处理的基础知识，我们还可以深入了解更多错误处理的技巧和方法。希望本文可以为你提供帮助，谢谢阅读！