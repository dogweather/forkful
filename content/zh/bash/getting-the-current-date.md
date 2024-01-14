---
title:                "Bash: 获取当前日期"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么使用Bash编程

Bash是一种功能强大的命令行解释器，它可以帮助用户在Unix和Linux操作系统中进行复杂的任务。获取当前日期可能听起来简单，但实际上它可以作为学习Bash编程的一个很好的起点。通过掌握获取当前日期的方法，您将能够学习和理解更高级的Bash编程概念，从而提高您的技能。

# 如何获取当前日期

在Bash中，您可以使用内置的`date`命令来获取当前日期。假设我们想要以`YYYY-MM-DD`的格式显示当前日期，那么可以使用以下命令：

```
Bash
date +"%Y-%m-%d"
```
这将在控制台中输出当前日期，例如`2021-09-28`。您还可以结合使用其他选项来自定义日期的格式，例如显示小时和分钟：

```
Bash
date +"%Y-%m-%d %H:%M"
```
这将输出类似于`2021-09-28 15:30`的结果。如果您想要获取当前日期的Unix时间戳，可以使用`%s`选项：

```
Bash
date +"%s"
```
这将输出自1970年1月1日以来的秒数。

# 深入了解获取当前日期

除了上述方法外，还有其他一些方法可以在Bash中获取当前日期。例如，您可以使用`printf`命令来格式化日期，或者使用`builtin`命令来执行与`date`类似的操作。另外，您还可以使用`calendar`命令来显示日历，或者安装`tmux`工具来在终端中实时显示当前日期。

无论您使用哪种方法，获取当前日期的基本概念都是一样的。首先，您需要知道日期的格式，然后使用特定的选项来格式化输出。这是学习Bash编程中重要的一步，因为您可以将此概念应用于其他方面。

# 查看更多

- [Bash官方文档](https://www.gnu.org/software/bash/)
- [Bash编程基础](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Bash编程指南](https://ryanstutorials.net/bash-scripting-tutorial/)
- [Bash日期和时间格式化指南](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
- [Bash日期和时间函数](https://www.learnshell.org/en/Bash_Functions#Date_and_Time)