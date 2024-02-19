---
aliases:
- /zh/c-sharp/interpolating-a-string/
date: 2024-01-20 17:50:34.988572-07:00
description: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u5C06\u53D8\u91CF\u548C\u8868\u8FBE\
  \u5F0F\u5D4C\u5165\u5230\u5B57\u7B26\u4E32\u5B57\u9762\u503C\u4E2D\u7684\u8FC7\u7A0B\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u7B80\u5316\u5B57\u7B26\
  \u4E32\u7684\u7EC4\u5408\u548C\u683C\u5F0F\u5316\uFF0C\u4F7F\u4EE3\u7801\u66F4\u52A0\
  \u6E05\u6670\u6613\u8BFB\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.120472
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u5C06\u53D8\u91CF\u548C\u8868\u8FBE\
  \u5F0F\u5D4C\u5165\u5230\u5B57\u7B26\u4E32\u5B57\u9762\u503C\u4E2D\u7684\u8FC7\u7A0B\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u7B80\u5316\u5B57\u7B26\
  \u4E32\u7684\u7EC4\u5408\u548C\u683C\u5F0F\u5316\uFF0C\u4F7F\u4EE3\u7801\u66F4\u52A0\
  \u6E05\u6670\u6613\u8BFB\u3002"
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
字符串插值是将变量和表达式嵌入到字符串字面值中的过程。程序员这样做是为了简化字符串的组合和格式化，使代码更加清晰易读。

## How to: (如何做：)
使用字符串插值，只需在字符串前加一个 `$` 符号，并将变量或表达式包围在 `{}` 中。

```C#
string name = "小明";
int age = 28;
string greeting = $"你好, {name}! 你今年 {age} 岁。";

Console.WriteLine(greeting);
```

输出：
```
你好, 小明! 你今年 28 岁。
```

## Deep Dive (深入探索)
字符串插值在 C# 6.0 中被引入，并且自那以后就是格式化字符串的首选方法。在此之前，程序员通常使用 `String.Format()` 方法或加号 (`+`) 连接变量到字符串中。

使用字符串插值，C# 编译器在幕后转换插值表达式为 `String.Format()` 调用。这意味着 `"Hello, {name}!"` 实际上是 `String.Format("Hello, {0}!", name)` 的现代化简写。字符串插值表达式中的代码在运行时计算，并且可以包括复杂的逻辑。

除了简单的变量替换，字符串插值也支持格式化。例如：

```C#
double price = 123.456;
string message = $"价格: {price:C}";

Console.WriteLine(message);
```
输出一个格式化的货币值：
```
价格: ¥123.46
```
在上面的例子中，`:C` 指定了货币格式。字符串插值不止是方便，它还具有提升代码性能的潜能，尤其是在处理大量的字符串拼接时。

## See Also (另请参阅)
- [字符串插值 (官方文档)](https://docs.microsoft.com/zh-cn/dotnet/csharp/language-reference/tokens/interpolated)
- [C# 字符串格式化](https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/composite-formatting)
