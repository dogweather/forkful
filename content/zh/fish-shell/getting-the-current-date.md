---
title:                "获取当前日期"
html_title:           "Fish Shell: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么以及为什么？

获取当前日期是指获取计算机系统在当前时刻的日期，通常是以年月日的格式显示。程序员经常需要获取当前日期来记录时间戳、创建文件名或者执行其他与日期相关的操作。

## 怎么做？

通过Fish Shell内置的`date`命令来获取当前日期。在终端中输入以下命令来查看当前日期：

```
Fish Shell date
```

输出可能类似于`2021年12月13日 星期一`，具体格式取决于你的系统设置。如果想要以其他格式展现日期，可以通过在`date`命令后加上选项来实现。例如，要以`月/日/年`的格式显示当前日期，可以输入以下命令：

```
Fish Shell date +"%m/%d/%y"
```

这将输出`12/13/21`。更多关于`date`命令的选项可以通过输入`man date`命令来查看使用手册。

## 深入了解

在UNIX系统中，日期被存储为从1970年1月1日起经过的秒数，这被称为UNIX时间戳。使用UNIX时间戳可以方便地对日期进行计算和比较。除了Fish Shell自带的`date`命令，还有其他一些UNIX工具可以用来获取当前日期，例如`hwclock`和`zic`。

## 参考资料

- [Fish Shell date命令文档](https://fishshell.com/docs/current/cmds/date.html)
- [UNIX时间戳介绍](https://en.wikipedia.org/wiki/Unix_time)