---
changelog:
- 2024-02-25, gpt-4-0125-preview, translated from English
date: 2024-02-25 17:06:53.421699-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u5728C#\u4E2D\uFF0C\u5B57\u7B26\u4E32\u63D2\
  \u503C\u901A\u8FC7\u4E00\u4E2A\u7F8E\u5143\u7B26\u53F7(`$`)\u8DDF\u968F\u4E00\u4E2A\
  \u5B57\u7B26\u4E32\u5B57\u9762\u91CF\u6765\u8868\u793A\u3002\u53D8\u91CF\u540D\u6216\
  \u8868\u8FBE\u5F0F\u88AB\u5305\u56F4\u5728\u82B1\u62EC\u53F7(`{}`)\u4E2D\u3002"
lastmod: '2024-04-05T21:53:48.067888-06:00'
model: gpt-4-0125-preview
summary: "\u5728C#\u4E2D\uFF0C\u5B57\u7B26\u4E32\u63D2\u503C\u901A\u8FC7\u4E00\u4E2A\
  \u7F8E\u5143\u7B26\u53F7(`$`)\u8DDF\u968F\u4E00\u4E2A\u5B57\u7B26\u4E32\u5B57\u9762\
  \u91CF\u6765\u8868\u793A\u3002\u53D8\u91CF\u540D\u6216\u8868\u8FBE\u5F0F\u88AB\u5305\
  \u56F4\u5728\u82B1\u62EC\u53F7(`{}`)\u4E2D\u3002"
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
