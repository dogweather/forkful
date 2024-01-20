---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？

获取当前日期是检索系统当前日期和时间的过程。程序员之所以需要做这个，主要是为了记录时间戳，跟踪错误，产生特定的日期/时间，或者用在定时任务等等。

## 如何操作：

在Bash中，我们可以使用`date`命令来获取当前的日期和时间。代码和样例输出如下：

```Bash
#!/bin/bash
date
```

这将返回当前的日期和时间。

```Bash
Sat Sep 19 21:16:06 UTC 2021
```

## 深入研究：

在古老的UNIX系统和Shell编程中，`date`就已经存在了。这种命令用于在程序中处理和格式化日期和时间。除了`date`命令，你也可以选择其他命令像`printf`或者特定的语言构造方式（像Python的datetime模块）来实现。

在Bash中，`date`命令接受`+`后接格式指示符参数，来控制显示的时间和日期的格式。例如：

```Bash
#!/bin/bash
date "+%Y-%m-%d"
```

将返回以下的格式:

```Bash
2021-09-19
```

## 查看其他：

在编程中获取当前日期是一个非常常见的需求，以下链接提供了更多的相关信息和样例：
- Bash `date` 命令手册：
    `https://man7.org/linux/man-pages/man1/date.1.html`
- `printf` 命令手册：
    `https://man7.org/linux/man-pages/man1/printf.1p.html`
- Python `datetime` 模块文档：
    `https://docs.python.org/3/library/datetime.html`
- 阮一峰的网络日志：Linux 命令行教程 - `date`命令：`http://www.ruanyifeng.com/blog/2015/05/date.html`