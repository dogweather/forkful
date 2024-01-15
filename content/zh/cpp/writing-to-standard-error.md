---
title:                "编写标准错误"
html_title:           "C++: 编写标准错误"
simple_title:         "编写标准错误"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么要写入标准错误？
Writing to standard error is a common practice in C++ programming. It allows developers to display error messages or log information while their program is running, making it easier to debug and troubleshoot any issues that may arise. 

# 如何写入标准错误
```C++
#include <iostream>
#include <cstdlib>

int main() {
    std::cerr << "This is an error message." << std::endl;
    return EXIT_FAILURE;
}
```

运行上面的程序，你会得到类似如下输出：

```
This is an error message.
```

## 深入探讨
标准错误流（standard error stream）是在C++标准输出流（standard output stream）之外的另一种输出通道。它可以用来显示错误信息或者记录程序运行过程中的一些重要信息。如果在C++程序中没有显式地指定输出到哪个流中，那么错误信息通常会被发送到标准错误流中。在调试和排查问题时，写入标准错误是一个非常有用的工具。

# 参考链接
- 关于C++标准错误流的更多信息，请参考[Cplusplus.com](https://www.cplusplus.com/reference/ios/cerr/)
- 在C++中输出错误信息的另一种方法是使用`cerr`和`clog`流，详见[Tutorialspoint](https://www.tutorialspoint.com/cplusplus/cpp_error_handling.htm)
- 查看本文中使用的代码样例的完整版本，请访问[Github](https://github.com/example)