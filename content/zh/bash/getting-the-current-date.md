---
title:    "Bash: 获取当前日期"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

Bash编程语言是Linux和Unix系统中最常用的脚本语言之一。通过使用Bash，我们可以自动化执行一个或多个命令，方便我们的日常工作。获取当前日期信息对于任务计划和日志记录都是非常有用的。让我们来深入了解如何使用Bash获取当前日期。

## 如何做

在Bash中，我们可以使用内置的`date`命令来获取当前日期信息。下面是一个使用`date`命令的例子，它会打印出当前的日期和时间：

```Bash
#!/bin/bash

today=$(date +"%Y-%m-%d %H:%M:%S")
echo "今天是 $today"
```

输出：今天是 2021-09-15 10:00:00

在上面的例子中，我们首先使用`date +"%Y-%m-%d %H:%M:%S"`来设置日期和时间的格式。使用`%Y`来表示年份，`%m`表示月份，`%d`表示日期，`%H`表示小时，`%M`表示分钟，`%S`表示秒。然后，我们将设置的格式赋值给变量`today`，并使用`echo`命令打印出来。

如果我们只想获取日期信息而不包括时间，可以使用`date +"%Y-%m-%d"`。如果想要获取其他格式的日期，可以查看`date`命令的帮助文档。

## 深入探讨

`date`命令可以获取当前日期信息之外，还可以用来进行日期的计算。下面是一个例子，我们使用`date`命令来计算一个月后的日期：

```Bash
#!/bin/bash

month_after=$(date -d "+1 month" +%Y-%m-%d)
echo "一个月后的今天是 $month_after"
```

输出：一个月后的今天是 2021-10-15

在上面的例子中，我们使用`date -d "+1 month"`来表示一个月后的日期，然后使用`+%Y-%m-%d`来设置输出的格式。

除了日期计算，`date`命令还可以用来设置系统的时间和日期，以及转换不同格式的日期。

## 参考链接

- [Bash文档](https://www.gnu.org/software/bash/)
- [Bash编程入门](https://wizardforcel.gitbooks.io/bashtutorial/content/)
- [Linux命令大全](https://www.linuxcool.com/)
- [Bash技巧和技巧](https://misc.flogisoft.com/bash/tip_colors_and_formatting)

## 参见

[日期和时间格式化](https://www.gnu.org/software/coreutils/manual/html_node/Date-conversion-specifiers.html#Date-conversion-specifiers)