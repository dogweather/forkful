---
date: 2024-01-26 01:17:35.384732-07:00
description: "\u91CD\u6784\u662F\u6307\u5728\u4E0D\u6539\u53D8\u8BA1\u7B97\u673A\u4EE3\
  \u7801\u5916\u90E8\u884C\u4E3A\u7684\u60C5\u51B5\u4E0B\u91CD\u65B0\u7EC4\u7EC7\u73B0\
  \u6709\u4EE3\u7801\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u8FDB\
  \u884C\u91CD\u6784\uFF0C\u662F\u4E3A\u4E86\u6E05\u7406\u4EE3\u7801\u3001\u589E\u5F3A\
  \u53EF\u8BFB\u6027\u3001\u51CF\u5C11\u590D\u6742\u6027\u4EE5\u53CA\u63D0\u9AD8\u53EF\
  \u7EF4\u62A4\u6027\u3002"
lastmod: '2024-03-11T00:14:21.561338-06:00'
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u6307\u5728\u4E0D\u6539\u53D8\u8BA1\u7B97\u673A\u4EE3\
  \u7801\u5916\u90E8\u884C\u4E3A\u7684\u60C5\u51B5\u4E0B\u91CD\u65B0\u7EC4\u7EC7\u73B0\
  \u6709\u4EE3\u7801\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u8FDB\
  \u884C\u91CD\u6784\uFF0C\u662F\u4E3A\u4E86\u6E05\u7406\u4EE3\u7801\u3001\u589E\u5F3A\
  \u53EF\u8BFB\u6027\u3001\u51CF\u5C11\u590D\u6742\u6027\u4EE5\u53CA\u63D0\u9AD8\u53EF\
  \u7EF4\u62A4\u6027\u3002"
title: "\u4EE3\u7801\u91CD\u6784"
---

{{< edit_this_page >}}

## 什么是重构 & 为什么要重构？

重构是指在不改变计算机代码外部行为的情况下重新组织现有代码的过程。程序员之所以进行重构，是为了清理代码、增强可读性、减少复杂性以及提高可维护性。

## 如何进行重构：

让我们重构一个简单的C#方法，该方法计算并打印一个数字数组的总和：

重构前：
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("The sum is " + sum);
    }
}
```

重构后：
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"The sum is {CalculateSum()}");
    }
}

// 使用方式：
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

通过重构，我们区分了关注点，使`Calculator`类更加灵活，可以接受任何数字数组，并利用LINQ使求和计算更加简洁。

## 深入了解

重构源于smalltalk编程社区，并在1990年代由Martin Fowler的书籍《重构：改善既有代码的设计》（Refactoring: Improving the Design of Existing Code）中得到推广。多年来，它已成为敏捷方法论和良好编码实践的基本部分。

重构有各种方法，例如测试驱动开发（TDD）中的红-绿-重构。它通过从一个失败的测试开始，让测试通过，然后清理代码，确保重构不会引入错误。

实施重构时，拥有一套全面的测试套件至关重要，以确保在过程中不会破坏任何功能。自动化重构工具，如用于C#的ReSharper，也可以通过提供安全的代码结构更改方式来帮助这一过程。然而，工具应仅作为对代码库和编码原则深入理解的补充。

## 另请参阅

- Martin Fowler的重构开创性作品：[重构：改善既有代码的设计](https://martinfowler.com/books/refactoring.html)
- 微软在Visual Studio中的重构指南：[重构 (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- 附有示例的重构模式详细解析：[SourceMaking重构](https://sourcemaking.com/refactoring)
