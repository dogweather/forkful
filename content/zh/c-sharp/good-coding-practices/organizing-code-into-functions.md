---
date: 2024-01-26 01:10:14.468997-07:00
description: "\u5C06\u4EE3\u7801\u5757\u5316\u4E3A\u51FD\u6570\uFF0C\u5C31\u50CF\u662F\
  \u5C06\u4E50\u9AD8\u79EF\u6728\u5206\u95E8\u522B\u7C7B\u5730\u653E\u5165\u4E0D\u540C\
  \u7684\u7BB1\u5B50\u4E2D\u2014\u2014\u8FD9\u8BA9\u6211\u4EEC\u66F4\u5BB9\u6613\u627E\
  \u5230\u5E76\u4F7F\u7528\u5B83\u4EEC\u3002\u6211\u4EEC\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u907F\u514D\u91CD\u590D\uFF0C\u7B80\u5316\u7406\u89E3\uFF0C\u5E76\u51CF\u5C11\
  \u7EF4\u62A4\u7684\u56F0\u6270\u3002"
lastmod: '2024-03-13T22:44:47.773592-06:00'
model: gpt-4-1106-preview
summary: "\u5C06\u4EE3\u7801\u5757\u5316\u4E3A\u51FD\u6570\uFF0C\u5C31\u50CF\u662F\
  \u5C06\u4E50\u9AD8\u79EF\u6728\u5206\u95E8\u522B\u7C7B\u5730\u653E\u5165\u4E0D\u540C\
  \u7684\u7BB1\u5B50\u4E2D\u2014\u2014\u8FD9\u8BA9\u6211\u4EEC\u66F4\u5BB9\u6613\u627E\
  \u5230\u5E76\u4F7F\u7528\u5B83\u4EEC\u3002\u6211\u4EEC\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u907F\u514D\u91CD\u590D\uFF0C\u7B80\u5316\u7406\u89E3\uFF0C\u5E76\u51CF\u5C11\
  \u7EF4\u62A4\u7684\u56F0\u6270\u3002"
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

## 何为与为何？
将代码块化为函数，就像是将乐高积木分门别类地放入不同的箱子中——这让我们更容易找到并使用它们。我们这样做是为了避免重复，简化理解，并减少维护的困扰。

## 如何操作：
想象一下，如果你有段代码需要打印几次问候语。没有函数，代码看起来就会是一团糟。有了函数，一切就变得整洁了。

```C#
// 没有函数 - 重复性的
Console.WriteLine("Hello, Amy!");
Console.WriteLine("Hello, Bob!");
Console.WriteLine("Hello, Charlie!");

// 有了函数 - 更清晰
void Greet(string name) {
    Console.WriteLine($"Hello, {name}!");
}

Greet("Amy");
Greet("Bob");
Greet("Charlie");
```

输出结果是相同的，但第二种方式更整洁。

## 深入探讨
回到早期，用汇编语言时，你得通过GOTO指令跳转到不同的代码位置——这既混乱又难以跟踪。函数则是一个重大的进步，好比是工具箱中有序的抽屉。还有其他选择吗？当然。比如方法，它是类上下文中的函数。然后是lambda表达式和内联函数, 用于快速的一次性任务。

关于实现——小而集中的功能是宝贵的。它们更容易进行测试和调试。而承担许多职责的大型函数可能会变得庞大无比，赢得了“意大利面条代码”的不光彩称号。坚持每个函数只做一件事；你以后会感激自己的。

## 另见
想了解更多关于函数和最佳实践，请查阅：

- 《代码整洁之道》作者：罗伯特·C·马丁：保持函数整洁的原则。
- 《重构》作者：马丁·福勒：改善现有代码的方法。
- 微软C#指南中关于方法的部分: https://docs.microsoft.com/zh-cn/dotnet/csharp/programming-guide/classes-and-structs/methods
