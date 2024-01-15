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

## Why

你可能听说过Bash是一种流行的编程语言，但是你可能还不知道它可以用来处理日期。在这篇文章中，我们将深入探讨如何将日期转换为字符串，以方便在脚本中使用。

## How To

使用`date`命令可以轻松地获取系统当前的日期和时间。但是，如果想将其用作字符串的一部分，就需要进行一些转换。下面是一个简单的示例，它将系统当前的日期转换为字符串格式：

```Bash
date_str=`date +"%Y-%m-%d"`
echo "今天的日期是：$date_str"
```

输出：

```
今天的日期是：2021-09-05
```

在这个示例中，我们使用`+%Y-%m-%d`来指定日期的格式，`%Y`代表年份，`%m`代表月份，`%d`代表日期。你也可以根据自己的需求来自定义日期的格式，例如：`date +"%b %d, %Y"`会得到类似于 "Sep 05, 2021" 的格式。

如果需要获取特定日期的字符串格式，而不是当前日期，可以在`date`命令后面加上`-d`选项，然后指定日期。例如，要获得今天两个月后的日期，可以这样做：

```Bash
date_str=`date -d "+2 months" +"%Y-%m-%d"`
echo "两个月后的日期是：$date_str"
```

输出：

```
两个月后的日期是：2021-11-05
```

除了使用`+%`来自定义日期的格式，你还可以使用不同的选项来获取不同的日期信息。例如，`date +"%A"`会得到今天是星期几的信息，`date +"%H:%M"`会得到当前的小时和分钟。你可以在bash的手册页面中查看所有可用选项的列表。

## Deep Dive

在Bash中，日期转换通常是通过`date`命令和字符串处理函数来完成的。在这篇文章中，我们只介绍了最基本的日期转换方法，但是你也可以通过使用其他的操作符和函数来实现更复杂的日期处理。

除了`date`命令外，Bash还有内置的`printf`函数，它可以使用类似于C语言中的`printf`函数的格式来打印输出。因此，你也可以使用类似于下面的语法来实现日期转换：

```Bash
printf -v date_str "%(%Y-%m-%d)T" -1
```

这个语法中，`-v`选项用来将返回值赋值给变量，`%(%Y-%m-%d)T`代表告诉`printf`函数使用指定格式来格式化当前的日期。

另外，如果你想在脚本中频繁使用日期转换，那么可以将它们封装成函数，以便在多处调用，例如：

```Bash
# 获取当前日期的字符串格式
get_date() {
    printf "%(%Y-%m-%d)T" -1
}

# 获取指定日期后几天的字符串格式
get_date_after() {
    date -d "$1 days" +"%Y-%m-%d"
}
```

你可以在脚本中使用这些函数来方便地进行日期转换。

## See Also
- [Bash Reference Manual: Date Manipulation](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion)
- [Bash Reference Manual: Printf](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)

希望这篇文章能够帮助你学习如何在Bash中转换日期为字符串。如果你想深入了解Bash的更多特性，请查看以上提供的链接。