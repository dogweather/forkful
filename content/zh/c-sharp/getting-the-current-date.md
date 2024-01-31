---
title:                "获取当前日期"
date:                  2024-01-20T15:13:38.147477-07:00
simple_title:         "获取当前日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? 什么 & 为什么？
在编程中获取当前日期是指获取当前的日期和时间信息。程序员这么做是因为很多应用程序需要用到这个数据，比如日志记录、数据戳标或时间计算。

## How to: 怎么做？
使用C#获取当前日期很简单。这是个例子：

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate.ToString("yyyy-MM-dd HH:mm:ss"));
    }
}
```

运行上面的代码，你会看到类似这样的输出：

```
2023-04-12 14:23:45
```

就是这样，日期和时间，清晰明了。

## Deep Dive 深入探究
`DateTime.Now`是.NET中用来获取当前本地时间的一个属性。历史上，.NET 开发者会使用`DateTime.Now`或者`DateTime.UtcNow` — 后者获取的是当前的世界协调时间（UTC）。选择哪一个取决于你的需要。

从.NET Core开始，越来越推荐使用`DateTimeOffset`，因为它在考虑时区的上下文中提供了更准确的时间点。不过，如果你仅仅需要一个日期戳，用`DateTime`就足够了。

在后台，`DateTime.Now`是通过调用操作系统的功能来获得当前时间的。在Windows中，这可能是通过`GetSystemTimeAsFileTime`函数。

有几种格式化日期的方法。你可以像上面的例子一样选择自己定义格式，或者使用标准的格式字符串，比如`.ToShortDateString()`或`.ToLongTimeString()`等。

## See Also 另请参阅
- [DateTime Struct (Microsoft Doc)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [DateTime versus DateTimeOffset (Microsoft Doc)](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime)
- [.NET Date and Time Format Strings (Microsoft Doc)](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings) 

这些链接会带你到更深层次的探究，有助于你全面理解日期和时间在.NET中的处理方式。
