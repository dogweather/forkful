---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么以及为什么？

获取当前日期是指编程代码为你提供今天的确切日期。程序员执行此操作的主要原因是跟踪、记录数据或日志，以及提供时间戳功能。

## 如何操作：

在Elixir中，我们依靠DateTime模块获取当前日期。以下为你操作的代码示例以及输出结果：

```Elixir
DateTime.now("Etc/UTC")
```

输出示例：

```Elixir
{:ok, #DateTime<2021-09-24 03:44:01.266302Z>}
```

上述代码段中，“Etc/UTC”是官方标准的世界时区，你可以根据需求更改为其他标准时区字符串。

## 深入了解

使用Elixir获取当前日期的历史并不是特别悠久。Elixir 1.3版本开始引入了DateTime模块，将这个功能更好地集成到语言中。

在获取日期时，还有一些替代方法，比如我们也可以使用Date.today函数：

```Elixir
Date.today
#输出示例：
#~D[2021-09-23]
```

这段代码会根据本地时区输出当前日期。

在Elixir中取得当前日期的实现细节主要依靠DateTime和Date模块，这两个模块都使用了内部结构，并考虑了各种可能的边界情况，以提供最准确的日期和时间。

## 另请参见

以下链接提供了Elixir获取当前日期的相关内容：

1. Elixir官方网站-DateTime：https://hexdocs.pm/elixir/DateTime.html
2. Elixir之PhO – 获取当前日期和时间：https://pragtob.wordpress.com/2017/04/17/getting-the-current-date-and-time-in-elixir-a-journey/
3. Elixir中的日期和时间：https://www.poeticoding.com/working-with-dates-and-times-in-elixir-with-elixir

这些链接让你更深入地了解Elixir中获取日期和时间的知识，并提供更多操作实例和教程。