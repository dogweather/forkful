---
title:                "获取当前日期"
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
获取当前日期是指从计算机系统中获取当前的日期和时间。程序员在编程中经常需要使用当前日期，并且通常会根据不同的需求来格式化日期和时间，比如将日期显示为特定格式或者计算两个日期之间的时间差。因此，获取当前日期对于程序员来说是非常重要的。

## 如何：
这里提供两种在Bash中获取当前日期的方法。

### 方法一：
```Bash
echo $(date)
```
#### 输出：
Thu Apr 29 19:23:44 EDT 2021

### 方法二：
```Bash
echo $(date +%m-%d-%Y)
```
#### 输出：
04-29-2021

## 深入了解：
获取当前日期这个概念是从计算机中出现的，特别是在许多操作系统支持的。它可以追溯到1970年，当时的Unix操作系统开发人员首先引进了一个UNIX时间戳，用于跟踪系统中过去和现在的时间。在Bash中，也可以使用其他命令来获取日期，如`cal`和`date +%s`，它们可以分别显示日历和时间戳。

除了使用Bash命令外，还可以使用其他编程语言来获取当前日期，如Python中的`datetime`模块和Java中的`Date`类。这些语言也提供了更多日期和时间的处理功能。

## 参考链接：
- [Bash文档](https://www.gnu.org/software/bash/manual/bash.html)
- [Unix时间戳](https://en.wikipedia.org/wiki/Unix_time)
- [Python datetime模块](https://docs.python.org/3/library/datetime.html)
- [Java Date类](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)