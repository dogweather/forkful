---
title:                "计算未来或过去的日期"
html_title:           "Bash: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么

## 为什么要计算未来或过去的日期

在日常生活中，我们经常需要计算未来或过去的日期，比如计划旅行、安排活动或预订会议。使用Bash可以方便地进行日期计算，节省我们的时间和精力。

# 如何进行

## 使用Bash进行日期计算

在Bash中，我们可以使用`date`命令来计算未来或过去的日期。下面是一个例子，我们想要计算今天的日期过去三天的日期：

```Bash
date -d "today - 3 days"
```

输出将会是过去三天的日期，例如 如果今天是2021年10月1日，那么输出将是2021年9月28日。

## 格式化输出日期

除了简单的日期计算，我们也可以对输出的日期进行格式化。比如，我们想要输出明天的日期，并以“月-日-年”的格式显示，我们可以使用以下命令：

```Bash
date -d "tomorrow" +"%m-%d-%Y"
```

输出将会是明天的日期，例如如果今天是2021年10月1日，那么输出将是10-02-2021。

# 深入探索

## 关于Bash日期计算的更多信息

Bash中的`date`命令非常灵活，它可以计算未来的日期、过去的日期，甚至是指定一个特定的日期进行计算。我们可以使用`man date`命令来查看更多关于`date`命令的信息。

此外，我们也可以使用其他工具来进行日期计算，比如`cal`命令可以输出日历，在日历中我们可以结合`day`命令来计算未来或过去的日期。如果你对时间戳有研究，也可以使用`date -d @timestamp`命令来进行日期转换。

# 查看也可以

## 查看这些链接来了解更多关于Bash日期计算的内容

- [Bash日期计算教程](https://www.linuxformat.com/tutorials/bash-date-manipulation/): 一篇详细的教程，介绍如何使用Bash进行日期计算。
- [使用Bash计算日期](https://www.gnu.org/savannah-checkouts/gnu/coreutils/manual/html_node/Example-of-date.html): GNU官方手册提供的一个日期计算的示例。
- [Linux日期格式化](https://www.baeldung.com/linux/date-formatting): 一篇关于如何在Linux中格式化日期的文章。