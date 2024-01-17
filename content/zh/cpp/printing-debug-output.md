---
title:                "打印调试输出"
html_title:           "C++: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

# 什么是打印调试输出？为什么程序员要这么做？

所谓打印调试输出，就是在程序代码中加入一些输出语句，用来帮助程序员追踪程序的运行过程。程序员通常会在程序中加入调试输出来帮助他们理解程序的执行流程，并找出可能存在的错误。

# 如何进行打印调试输出？

```C++
#include <iostream> // 包含标准输入输出库

int main() {
    int num1 = 10; // 定义一个整型变量
    int num2 = 5; // 定义另一个整型变量

    std::cout << "num1的值为： " << num1 << std::endl; // 使用cout语句打印调试信息
    std::cout << "num2的值为： " << num2 << std::endl; // 同样地，打印另一个变量的值
    std::cout << "num1 + num2的结果为： " << num1 + num2 << std::endl; // 打印计算结果

    return 0; // 返回程序执行的结果，0代表成功
}
```

样例输出：
```
num1的值为： 10
num2的值为： 5
num1 + num2的结果为： 15
```

# 深入了解打印调试输出

打印调试输出是一种古老但常用的调试手段，在早期的编程语言中已经存在。现在，也有一些替代的调试方法，比如使用断点调试。但是，打印调试输出仍然是最简单直接的方式，能够帮助程序员快速定位程序中可能存在的问题。在实际开发中，通常建议将调试输出移除或注释掉，避免对程序性能造成影响。

# 参考资料

- [C++教程 - 标准库的输入输出](https://www.runoob.com/cplusplus/cpp-standard-library-input-output.html)
- [如何实现高效的C++调试输出](https://www.codenong.com/js8f860d1f629a/)
- [调试方法综述：断点调试、单步调试、打印调试等](https://blog.csdn.net/xiaoming100001/article/details/53047623)