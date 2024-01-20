---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？

将日期转换为字符串是一种常见的编程任务，它将日期数据转化成文本形式。“为什么要做呢？”主要是为了在用户界面或日志文件中显示日期。

## 如何做：

使用C#中的 `DateTime.ToString()` 方法可以轻松完成转换。下面是一个基础示例：

```C#
DateTime dt = DateTime.Now; // 获取当前日期和时间
string strDate = dt.ToString("MM/dd/yyyy"); // 日期格式转化
Console.WriteLine(strDate);  // 输出转化后的日期字符串
```

这将输出如下所示的日期：

```C#
"04/12/2021"
```

## 深度剖析

在开发初期，日期通常以字符串形式储存，然而这种方式在处理日期相关操作时效率低下，于是后来的语言版本加入了对日期类型的支持。在C#中，除了我们刚刚用到的具名格式化之外，还常用到一些标准日期和时间格式串。

例如，使用 `"D"` 可以得到完整的日期格式：

```C#
string strFullDate = dt.ToString("D");
Console.WriteLine(strFullDate);
```

这会输出：

```C#
"Thursday, April 12, 2021"
```

文件处理、数据库操作等场景下，日期字符串受到广泛应用。实现方式不一，利弊各异，总之应根据应用需求来选用。

## 另请参阅

- [C# 中的日期和时间格式化](https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [C# DateTime ToString() 方法](https://www.dotnetperls.com/datetime-tostring)
- [维基百科：日期表示法](https://zh.wikipedia.org/wiki/%E6%97%A5%E6%9C%9F%E8%A1%A8%E7%A4%BA%E6%B3%95)