---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？

日期转换为字符串是将数字格式的日期信息转化为我们能理解的文本形式。这样做可以将日期信息以更符合人类阅读习惯的方式展示出来，也便于进行日期信息的存储和传输。

## 如何做：

你可以使用Bash内置的`date`命令来将日期转换为字符串。看下面的例子：

```Bash
# 当前日期和时间
date_str=$(date)
echo $date_str
```

输出的样本:

```
Tue Sep 2 20:20:20 PST 2020
```

你也可以自定义日期的格式，例如：

```Bash
# 自定义日期格式
date_str=$(date +"%Y-%m-%d %H:%M:%S")
echo $date_str
```

输出的样本：

```
2020-09-02 20:20:20
```

## 深入探索

虽然Bash的 date 命令可以满足大部分的日期字符串转换需求，但它也有局限性。date命令无法处理多于4位的年份或不符合常规日期格式的输入值。对于这些情况，我们可能需要其它的工具或方法。

历史上，尽管日期和时间处理在计算机科学中一直是个棘手的问题，但是各种语言和工具都提供了丰富的库来支持。例如Perl、Python和Java都具有功能强大的日期处理库。

对于更复杂的日期字符串转换问题，你可能会需要使用这些库或者类似的工具来解决。

## 另请参阅

1. Bash手册中的[`date`命令](https://www.gnu.org/software/bash/manual/bash.html#Shell-Builtin-Commands)的详细说明。

2. Linux命令行的[日期与时间处理](https://ryanstutorials.net/linuxtutorial/scripting.php#dates)

3. [POSIX](https://pubs.opengroup.org/onlinepubs/009695399/utilities/date.html)规范定义的日期命令。