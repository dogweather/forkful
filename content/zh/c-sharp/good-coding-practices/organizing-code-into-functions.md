---
date: 2024-01-26 01:10:14.468997-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u60F3\u8C61\u4E00\u4E0B\uFF0C\u5982\u679C\
  \u4F60\u6709\u6BB5\u4EE3\u7801\u9700\u8981\u6253\u5370\u51E0\u6B21\u95EE\u5019\u8BED\
  \u3002\u6CA1\u6709\u51FD\u6570\uFF0C\u4EE3\u7801\u770B\u8D77\u6765\u5C31\u4F1A\u662F\
  \u4E00\u56E2\u7CDF\u3002\u6709\u4E86\u51FD\u6570\uFF0C\u4E00\u5207\u5C31\u53D8\u5F97\
  \u6574\u6D01\u4E86\u3002"
lastmod: '2024-04-05T22:38:46.935919-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u60F3\u8C61\u4E00\u4E0B\uFF0C\u5982\u679C\
  \u4F60\u6709\u6BB5\u4EE3\u7801\u9700\u8981\u6253\u5370\u51E0\u6B21\u95EE\u5019\u8BED\
  \u3002\u6CA1\u6709\u51FD\u6570\uFF0C\u4EE3\u7801\u770B\u8D77\u6765\u5C31\u4F1A\u662F\
  \u4E00\u56E2\u7CDF\u3002\u6709\u4E86\u51FD\u6570\uFF0C\u4E00\u5207\u5C31\u53D8\u5F97\
  \u6574\u6D01\u4E86\u3002"
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

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
