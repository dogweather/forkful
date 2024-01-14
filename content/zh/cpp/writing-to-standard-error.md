---
title:    "C++: 向标准错误输出写入数据"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

为什么：为什么要将数据写入标准错误？

很多时候在编写C++程序时，我们需要向用户提供一些必要的信息，例如运行错误的详细信息或者简单的状态更新。这些信息不适合被打印到标准输出，因为它们可能会干扰程序的正常输出。所以，我们需要一个方法来打印这些信息，但又不影响程序的运行。这就是写入标准错误的重要性所在。

如何做到：通过下面这些编程示例，让我们来看看如何将数据写入标准错误。

```C++
#include <iostream>

using namespace std;

int main() {
    cerr << "这条信息会被写入标准错误" <<endl;
    return 0;
}
```

输出: 

`这条信息会被写入标准错误`

我们可以看到，使用`cerr`关键字可以将数据直接写入标准错误。同时，`endl`用于换行，可以使得信息更加清晰易读。

深入了解：除了`cerr`关键字之外，C++还提供了其他两个用于写入错误信息的关键字，分别是`cerr`和`clog`。它们的主要区别在于缓冲机制。`cout`是立即写入数据，`cerr`是无缓冲的意思，而`clog`则是有缓冲的，即会在一定的条件下才写入数据。这样的设计可以提高程序的效率，并且可以灵活调整数据的输出时间。

另外，当数据量较大时，我们也可以选择将错误信息写入一个单独的文件中，这样可以更好地调试程序，同时避免混淆标准输出和标准错误的信息。

总之，通过将数据写入标准错误，我们可以更加灵活地控制程序的输出，同时避免造成不必要的干扰。

*## 参考链接*

1. [C++标准库文档](https://zh.cppreference.com/w/cpp/io/cerr)
2. [C++参考手册](https://www.cplusplus.com/reference/ios/ios/)
3. [C++错误输出技巧](https://www.geeksforgeeks.org/error-handling-c-programs/)
4. [C++标准库入门](http://c.biancheng.net/cpp/20/)