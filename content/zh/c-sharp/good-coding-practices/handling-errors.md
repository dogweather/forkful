---
date: 2024-01-26 00:50:53.813829-07:00
description: "\u5728C#\u4E2D\u5904\u7406\u9519\u8BEF\u662F\u7BA1\u7406\u610F\u5916\
  \u60C5\u51B5\u7684\u8FC7\u7A0B\u2014\u2014\u5C31\u50CF\u88AB\u81EA\u5DF1\u7684\u978B\
  \u5E26\u7ECA\u5012\u4E00\u6837\u3002\u7A0B\u5E8F\u53EF\u80FD\u4F1A\u56E0\u4E3A\u9519\
  \u8BEF\u7684\u6570\u636E\u6216\u4E0D\u7A33\u5B9A\u7684\u8FDE\u63A5\u51FA\u9519\u3002\
  \u6211\u4EEC\u5904\u7406\u9519\u8BEF\u662F\u4E3A\u4E86\u9632\u6B62\u6211\u4EEC\u7684\
  \u8F6F\u4EF6\u6454\u4E2A\u72D7\u5543\u6CE5\uFF0C\u8BA9\u5B83\u80FD\u591F\u4F18\u96C5\
  \u5730\u6062\u590D\u3002"
lastmod: '2024-03-13T22:44:47.775721-06:00'
model: gpt-4-1106-preview
summary: "\u5728C#\u4E2D\u5904\u7406\u9519\u8BEF\u662F\u7BA1\u7406\u610F\u5916\u60C5\
  \u51B5\u7684\u8FC7\u7A0B\u2014\u2014\u5C31\u50CF\u88AB\u81EA\u5DF1\u7684\u978B\u5E26\
  \u7ECA\u5012\u4E00\u6837\u3002\u7A0B\u5E8F\u53EF\u80FD\u4F1A\u56E0\u4E3A\u9519\u8BEF\
  \u7684\u6570\u636E\u6216\u4E0D\u7A33\u5B9A\u7684\u8FDE\u63A5\u51FA\u9519\u3002\u6211\
  \u4EEC\u5904\u7406\u9519\u8BEF\u662F\u4E3A\u4E86\u9632\u6B62\u6211\u4EEC\u7684\u8F6F\
  \u4EF6\u6454\u4E2A\u72D7\u5543\u6CE5\uFF0C\u8BA9\u5B83\u80FD\u591F\u4F18\u96C5\u5730\
  \u6062\u590D\u3002."
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 是什么 & 为什么？

在C#中处理错误是管理意外情况的过程——就像被自己的鞋带绊倒一样。程序可能会因为错误的数据或不稳定的连接出错。我们处理错误是为了防止我们的软件摔个狗啃泥，让它能够优雅地恢复。

## 如何操作：

我们从try-catch块开始。这就像在走钢丝的人下面放置一个安全网。如果他们滑倒了，他们不会坠落——他们会被捕获。

```C#
using System;

class ErrorHandlingExample {
    static void Main() {
        try {
            int[] numbers = {1, 2, 3};
            Console.WriteLine(numbers[5]);  // 哎呀，索引越界了！
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("捕获到一个错误：" + e.Message);
        }
    }
}
```

当事情走向不利时的示例输出：
```
捕获到一个错误：索引超出了数组界限。
```

现在我们添加一个finally块——不管发生什么，它都会发生，就像缴税一样。

```C#
try {
    // 这里可能会有问题的代码
} catch (SomeSpecificException e) {
    // 在这里处理那个特定的错误
} finally {
    // 无论上面发生了什么，这段代码都会运行
    Console.WriteLine("这总是运行。");
}
```

## 深入了解

错误处理自C#诞生之初就一直存在。随着时间的推移，它已经发展。早期，程序员依赖返回代码或全局标志来表示问题——笨拙且容易出错。

C#使用异常，这是一种更现代的方法。当不期望的事情发生时，就会抛出一个异常，就像在足球比赛中扔出一个旗帜一样。结构化的异常处理，使用try、catch和finally块，使得管理这些时刻比老式的错误检查更清晰、更干净。

有替代方案吗？当然。对于那些漏网之鱼，有`UnhandledExceptionEventHandler`。或者在异步代码中，错误处理会变得有些不同，使用的是带有自己异常包袱的`Task`对象。

实现细节——就像细则一样——很重要。异常可能很昂贵，如果随意抛出，会拉低性能。因此，我们只在异常情况下使用它们，而不是日常逻辑控制。

## 另请参阅

- [C#中异常的官方文档](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [C#异常处理的最佳实践](https://docs.microsoft.com/en-us/dotnet/standard/exceptions/best-practices-for-exceptions)
