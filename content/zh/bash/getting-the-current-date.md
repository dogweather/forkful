---
title:                "Bash: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

在Bash编程中，获取当前日期是一项非常常见的任务。它可以帮助你创建基于当前日期的文件名，或者作为一部分作业任务，以发送带有当前日期的邮件。不管你的目的是什么，获取当前日期都是一个很有用的工具。

## 如何做

在Bash中获取当前日期非常容易。你只需要使用内置的date命令，并选择你想要的日期格式。以下是一个简单的例子：

```Bash
DATE=$(date '+%Y-%m-%d')
echo $DATE
```

这个例子将打印出类似于“2021-07-19”的日期格式。你可以根据你的需要选择不同的格式，比如“%A”将会打印出星期几， “%H:%M:%S”将会打印出当前时间。你可以在不同的选项之间使用“-”符号来组合不同的日期格式。

## 深入了解

要更深入地了解如何使用date命令来获取当前日期，你可以查看它的手册页（man page）来获得更多的选项和用法。在Bash编程中，了解如何使用各种工具非常重要，它可以帮助你提高你的编程技能，并帮助你更有效地完成任务。

另外，你也可以使用其他工具来获取当前日期，比如使用Python中的datetime模块。不同的工具可以提供不同的功能和用法，你可以根据自己的喜好来选择最合适的工具。

## 另请参阅

- [Bash中的date命令手册页](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Python中的datetime模块文档](https://docs.python.org/3/library/datetime.html)