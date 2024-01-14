---
title:    "Bash: 将日期转换为字符串"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##为什么
在Bash编程中，日期是一个常见的数据类型，而将日期转换成字符串往往是必须要做的一步。通过转换日期为字符串，我们可以更方便地使用和处理日期数据，从而完成更复杂的任务。

##如何
在Bash中，使用`date`命令可以获取当前日期和时间，但是默认的输出格式并不是字符串。为了将日期转换为字符串，我们可以使用`date`命令的`+%s`选项，它可以将日期转换为秒数，并使用`date`命令的`-d`选项将秒数转换为可读的字符串。
```Bash
date +%s
# output: 1599258452
date -d @1599258452
# output: Mon Sep 07 15:47:32 PDT 2020
```
我们也可以使用`date`命令的`+%Y-%m-%d`选项来指定日期的输出格式，例如`date +%Y-%m-%d`会输出`2020-09-07`。
```Bash
date +%Y-%m-%d
# output: 2020-09-07
```

##深入探讨
在Bash中，日期是以秒数的形式存储的，而秒数是一个从1970年1月1日开始累积的数字。通过使用`date`命令的`+%s`选项，我们可以将日期转换为秒数，并通过`date`命令的`-d`选项将秒数转换为可读的字符串。同时，通过指定不同的格式选项，我们可以自定义日期的输出格式。

##参考链接
- [Bash中的日期处理](https://www.gnu.org/software/bash/manual/bash.html#Date-Manipulation)
- [date命令文档](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)
- [bash教程](https://www.liaoxuefeng.com/wiki/1417827820813862)