---
title:    "C++: 写入标准错误"
keywords: ["C++"]
---

{{< edit_this_page >}}

为什么：

在C++编程中，我们经常需要输出程序的一些信息，比如错误提示或者调试信息。使用标准输出函数可以将这些信息打印在终端上，但是如果我们想将这些信息保存到日志文件中，就需要用到标准错误输出函数。

## 如何使用

使用标准错误输出函数很简单，只需要调用`std::cerr`对象的`<<`运算符，然后将要输出的信息作为参数传入即可。例如：

```C++
#include <iostream>

int main() {
  std::string error_message = "Something went wrong!";
  std::cerr << error_message << std::endl;
  return 0;
}
```

输出结果为：

```
Something went wrong!
```

## 深入探讨

标准错误输出实际上是一个流对象，和标准输出`std::cout`类似。它们的区别在于，标准错误输出会将信息直接输出到终端，而标准输出会将信息缓存起来，需要在程序结束时才会输出。

另外，标准错误输出也可以结合重定向来将信息保存到日志文件中。比如在终端输入`./my_program 2> error.log`，就会将标准错误输出的信息保存在`error.log`文件中。

## 参考资料

- [C++ 标准错误输出函数cerr的使用](https://www.runoob.com/cplusplus/cpp-output.html)
- [C++ tutorial: Standard error](https://www.learncpp.com/cpp-tutorial/18-1-c-standard-error/)
- [c++ - what is the use of cerr in c++?](https://stackoverflow.com/questions/15869092/what-is-the-use-of-cerr-in-c)

## 另请参阅

- [使用 C++ 异常处理来提高代码的健壮性](https://www.runoob.com/w3cnote/cpp-exception-handling-to-improve-code-robustness.html)
- [学习 C++ 标准库中的 I/O 类](https://www.runoob.com/w3cnote/cpp-std-io-class.html)
- [C++ 中的标准输出、输入以及流操作符](https://www.cnblogs.com/blog-Aloha/p/9904725.html)