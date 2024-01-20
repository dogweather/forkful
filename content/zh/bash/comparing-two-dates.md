---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么&为什么？

日期比较是一种检查两个日期哪个先，哪个后的方法。程序员之所以会这么做，是因为在各种场景中，比如日志分析、时间管理应用等等，都需要这种功能。

## 如何操作：

我们使用Bash的`date`命令和`-d`选项。我们也需要借助于比较运算符。

```Bash
#!/bin/bash

# 定义两个日期
date1=$(date -d "2022-01-01" +%s)
date2=$(date -d "2022-02-01" +%s)

# 比较日期
if [ $date1 -lt $date2 ];
then
    echo "date1早于date2"
else
    echo "date1晚于date2"
fi
```
在这个例子中，你将看到输出`date1早于date2`，因为2022年1月1日确实早于2022年2月1日。

## 深入探索：

Bash `date`命令已在UNIX早期版本就出现了。`-d`选项可以 解析各种日期格式，包括英文描述，如"next Friday"。

除了`date`命令，我们还有其他方式进行日期比较，比如使用Perl、Python或Ruby等语言的日期函数。但是Bash内置的`date`通常会更便捷。

对于日期比较的实现，核心是把日期转换为从某个点（如1970年1月1日）开始的秒数，然后比较这两个数字。

## 延伸阅读：

- Bash `date`命令使用手册：[https://www.man7.org/linux/man-pages/man1/date.1.html](https://www.man7.org/linux/man-pages/man1/date.1.html)
- Bash编程指南：[https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)