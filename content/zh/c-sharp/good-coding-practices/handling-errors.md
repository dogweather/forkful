---
date: 2024-01-26 00:50:53.813829-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u6211\u4EEC\u4ECEtry-catch\u5757\u5F00\
  \u59CB\u3002\u8FD9\u5C31\u50CF\u5728\u8D70\u94A2\u4E1D\u7684\u4EBA\u4E0B\u9762\u653E\
  \u7F6E\u4E00\u4E2A\u5B89\u5168\u7F51\u3002\u5982\u679C\u4ED6\u4EEC\u6ED1\u5012\u4E86\
  \uFF0C\u4ED6\u4EEC\u4E0D\u4F1A\u5760\u843D\u2014\u2014\u4ED6\u4EEC\u4F1A\u88AB\u6355\
  \u83B7\u3002"
lastmod: '2024-03-13T22:44:47.775721-06:00'
model: gpt-4-1106-preview
summary: "\u6211\u4EEC\u4ECEtry-catch\u5757\u5F00\u59CB\u3002\u8FD9\u5C31\u50CF\u5728\
  \u8D70\u94A2\u4E1D\u7684\u4EBA\u4E0B\u9762\u653E\u7F6E\u4E00\u4E2A\u5B89\u5168\u7F51\
  \u3002\u5982\u679C\u4ED6\u4EEC\u6ED1\u5012\u4E86\uFF0C\u4ED6\u4EEC\u4E0D\u4F1A\u5760\
  \u843D\u2014\u2014\u4ED6\u4EEC\u4F1A\u88AB\u6355\u83B7."
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

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
