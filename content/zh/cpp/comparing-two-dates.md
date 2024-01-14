---
title:    "C++: 比较两个日期"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么要比较两个日期

日期在计算机编程中经常被使用，它们可以帮助我们记录事件发生的顺序、计算时间间隔等。因此，比较两个日期也是非常常见的操作。通过比较两个日期，我们可以判断哪个日期在另一个日期之前或之后，从而帮助我们做出相应的处理。

## 如何比较两个日期

在C++中，比较两个日期可以通过使用`std::chrono`库中的`time_point`类来实现。首先需要包含`chrono`头文件，然后定义两个日期变量为`time_point`类型，并给它们赋予对应的值，例如：

```C++
#include <chrono>

std::chrono::time_point<std::chrono::system_clock> date1 = std::chrono::system_clock::now();
std::chrono::time_point<std::chrono::system_clock> date2 = std::chrono::system_clock::now();
```

然后，我们可以使用`operator>`或`operator<`来比较两个日期的大小，例如：

```C++
if (date1 > date2) {
    std::cout << "date1 is after date2";
} else if (date2 > date1) {
    std::cout << "date2 is after date1";
} else {
    std::cout << "date1 and date2 are the same";
}
```

输出结果将根据日期的大小而不同。除了比较两个日期之外，我们也可以使用`operator==`来检查两个日期是否相同。

## 深入了解比较两个日期

在C++中，`time_point`类实际上是一个模板类，它可以接收多种不同的参数类型。例如，在上面的例子中，我们使用的是`system_clock`类作为`time_point`的参数，表示以系统时钟为基准的日期和时间。除此之外，还有其他的类如`steady_clock`和`high_resolution_clock`，它们都可以用来创建`time_point`对象。不同的时钟类有不同的精度和使用范围，具体可以查看C++的官方文档来了解更多。

此外，在比较日期的过程中，可能会遇到时区的问题。在C++11之前，处理日期和时间时会跟操作系统的时区设置相关，可能会导致一些不一致性。但是在C++11之后，引入了`std::chrono::utc_clock`类，用来表示协调世界时（UTC）的日期和时间，可以有效解决跨时区的问题。

## 查看更多资料

- [C++标准库中关于chrono的文档](https://en.cppreference.com/w/cpp/chrono)
- [C++11中的chrono库：一个真正的类型安全的日期和时间库](https://www.cnblogs.com/liangliang-dashen/p/13592470.html)
- [深入理解C++的系统时间抽象](https://www.zhihu.com/people/xie-yu-jie-43)
- [C++日期和时间操作技巧总结](https://www.jianshu.com/p/45945621cd5b)

## 参考链接

- [stackoverflow: Comparing two dates in C++](https://stackoverflow.com/questions/327365/compare-two-dates-in-c)
- [C++ Reference: chrono](https://en.cppreference.com/w/cpp/chrono)