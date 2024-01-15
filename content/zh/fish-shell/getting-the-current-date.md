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

## 为什么

我们常常需要知道当前的日期，例如在编写日记或者记录某些事件的时候。使用Fish Shell可以轻松获取当前日期，让我们来学习如何做到这一点。

## 如何做到

首先，在Fish Shell中打开一个终端窗口。然后，输入以下命令来获取当前日期：

```Fish Shell
date
```

你会看到类似于这样的输出：

```Fish Shell
Tue Mar 23 21:24:55 CST 2021
```

这是当前时间的标准格式，但是如果你想要以不同的格式显示，可以使用一些额外的参数。例如，如果你只想显示日期，可以添加`+"%d"`：

```Fish Shell
date +"%d"
```

这将输出当前日期的数字格式，例如`23`。你也可以添加其他参数来显示月份、年份、甚至自定义格式。有关更多详细信息，请参考Fish Shell的文档。

## 深入探讨

实际上，Fish Shell使用的`date`命令实际上是从UNIX操作系统中继承而来的。它是一个非常有用的工具，在工作中使用时可以帮助我们轻松获取当前日期，而不用担心繁琐的格式转换。

另外，你还可以在Fish Shell中使用`cal`命令来显示当前的日历。例如，输入`cal 3 2021`可以显示2021年3月的日历。这也是一个很实用的技巧，可以帮助我们更好地计划我们的日程安排。

## 参考链接

- Fish Shell官方文档：https://fishshell.com/docs/current/index.html
- UNIX操作系统：https://en.wikipedia.org/wiki/Unix
- `date`命令的详细参数文档：https://man7.org/linux/man-pages/man1/date.1.html