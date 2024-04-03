---
changelog:
- 2024-02-25, gpt-4-0125-preview, translated from English
date: 2024-02-25 17:06:53.421699-07:00
description: "\u5728C#\u4E2D\uFF0C\u5B57\u7B26\u4E32\u63D2\u503C\u5141\u8BB8\u4F60\
  \u901A\u8FC7\u5728\u5B57\u7B26\u4E32\u5B57\u9762\u91CF\u4E2D\u5305\u542B\u8868\u8FBE\
  \u5F0F\u6765\u521B\u5EFA\u65B0\u5B57\u7B26\u4E32\uFF0C\u8FD9\u4F7F\u5F97\u683C\u5F0F\
  \u5316\u548C\u8FDE\u63A5\u5B57\u7B26\u4E32\u53D8\u5F97\u66F4\u7B80\u5355\u3002\u7A0B\
  \u5E8F\u5458\u4EEC\u4F7F\u7528\u8FD9\u4E2A\u7279\u6027\u6765\u63D0\u9AD8\u4EE3\u7801\
  \u7684\u53EF\u8BFB\u6027\u548C\u53EF\u7EF4\u62A4\u6027\uFF0C\u7279\u522B\u662F\u5728\
  \u5904\u7406\u52A8\u6001\u5B57\u7B26\u4E32\u5185\u5BB9\u65F6\u3002"
lastmod: '2024-03-13T22:44:47.751855-06:00'
model: gpt-4-0125-preview
summary: "\u5728C#\u4E2D\uFF0C\u5B57\u7B26\u4E32\u63D2\u503C\u5141\u8BB8\u4F60\u901A\
  \u8FC7\u5728\u5B57\u7B26\u4E32\u5B57\u9762\u91CF\u4E2D\u5305\u542B\u8868\u8FBE\u5F0F\
  \u6765\u521B\u5EFA\u65B0\u5B57\u7B26\u4E32\uFF0C\u8FD9\u4F7F\u5F97\u683C\u5F0F\u5316\
  \u548C\u8FDE\u63A5\u5B57\u7B26\u4E32\u53D8\u5F97\u66F4\u7B80\u5355\u3002\u7A0B\u5E8F\
  \u5458\u4EEC\u4F7F\u7528\u8FD9\u4E2A\u7279\u6027\u6765\u63D0\u9AD8\u4EE3\u7801\u7684\
  \u53EF\u8BFB\u6027\u548C\u53EF\u7EF4\u62A4\u6027\uFF0C\u7279\u522B\u662F\u5728\u5904\
  \u7406\u52A8\u6001\u5B57\u7B26\u4E32\u5185\u5BB9\u65F6\u3002."
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

## 如何操作:
在C#中，字符串插值通过一个美元符号(`$`)跟随一个字符串字面量来表示。变量名或表达式被包围在花括号(`{}`)中。

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"你好, {name}! 你今年 {age} 岁。";
Console.WriteLine(interpolatedString);
// 输出: 你好, Jane! 你今年 28 岁。
```

在更复杂的例子中，你可以在花括号内执行操作或调用方法：

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"总价: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// 输出: 总价: $59.97
```
花括号内的`:C2`格式说明符将该数字格式化为带有两位小数的货币格式。

对于需要更高级格式化或本地化的场景，你可能会考虑使用`string.Format`方法或像Humanizer这样的库。Humanizer可以以更易读的格式管理和显示字符串、日期、时间、时间跨度、数字和数量。以下是使用Humanizer进行复杂字符串操作的一个例子。请注意，Humanizer不是.NET标准库的一部分，需要安装NuGet包`Humanizer`。

首先，通过NuGet安装Humanizer：

```
Install-Package Humanizer
```

然后，你可以如下使用它：

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"该事件发生在 {dayDifference} 天前。".Humanize();
Console.WriteLine(humanized);
// 根据配置和文化环境，可能的输出：该事件发生在 5 天前。
```

这个例子演示了基本用法。Humanizer支持广泛的功能性，可以应用于字符串、日期、数字等，使你的应用更加易于访问和直观。
