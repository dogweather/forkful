---
title:                "C++: 打印调试输出"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

为什么：调试输出是C++编程中必不可少的一部分，它可以帮助程序员找出代码中的错误，提高程序的可读性和可维护性。

如何实现：在C++中，我们可以使用cout语句来输出调试信息。例如，在下面的代码块中，我们使用cout来打印一个简单的字符串信息，并在屏幕上显示出来。

```C++
#include <iostream>
using namespace std;

int main() {
    cout << "Hello World!" << endl;
    return 0;
}
```

输出结果为：

```
Hello World!
```

深入探讨：除了简单的字符串输出，我们还可以使用cout打印变量的值、数据类型、数组、结构体等各种调试信息。我们也可以使用endl来换行，使用setw来设置输出宽度，使用setprecision来控制输出精度等等。总的来说，cout语句提供了很多灵活的选项来帮助我们对程序进行调试。

另外，我们也可以使用cerr来输出错误信息，它会在控制台显示红色的文字，并且会在程序崩溃时提供更详细的错误信息，帮助我们更快地找出问题所在。

和cout类似的还有clog，它可以用来输出程序运行时的一般性信息。和cerr不同的是，clog的信息会显示为黄色的文字，用来区分不同类型的输出信息。

总的来说，调试输出是一个非常重要的工具，它可以帮助我们更快地找出程序中的问题，并且提高我们的开发效率。

另请参阅：

- [C++ Output](https://www.programiz.com/cpp-programming/input-output)
- [C++ Debugging](https://www.tutorialspoint.com/cplusplus/cpp_debugging.htm)
- [The Power of Debugging: A Guide to Becoming a Better Programmer](https://www.codingblocks.net/blog/the-power-of-debugging/)
- [常见的C++编程错误及其解决方法](https://developer.mozilla.org/zh-CN/docs/Web/CSS/Common_CSS_Questions)