---
title:                "把日期转换成字符串"
html_title:           "Fish Shell: 把日期转换成字符串"
simple_title:         "把日期转换成字符串"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

很多时候，我们需要将日期转换成字符串以便在程序中使用。比如，需要将当前日期作为文件名保存，或者将日期作为一部分显示在网页中。在这篇文章中，我们将学习如何使用Fish Shell来完成这个任务。

## 如何进行

首先，我们需要使用Fish Shell自带的`date`命令来获取当前日期。然后，我们可以使用`string`命令来将日期转换成字符串。下面是一个示例代码及输出：

```
Fish Shell Code:
set current_date (date +%Y%m%d)
echo $current_date

Output:
20191126
```

在上面的代码中，我们使用了`set`命令来创建一个变量`current_date`来保存日期。然后使用`date`命令和`+%Y%m%d`参数来获取当前日期，并赋值给变量`current_date`。最后使用`echo`命令打印出变量的值。

接下来，我们可以使用`string`命令来将日期转换成字符串。下面是一个示例代码及输出：

```
Fish Shell Code:
string replace $current_date %Y%m%d "2020-01-01"

Output:
2020-01-01
```

在上面的代码中，我们使用`string replace`命令来替换日期变量的格式。第一个参数是变量名，第二个参数是要替换的格式，第三个参数是替换后的格式。

## 深入探讨

在上面的示例中，我们使用的是`%Y%m%d`格式来表示日期，其中`%Y`表示四位数的年份，`%m`表示两位数的月份，`%d`表示两位数的日期。你也可以使用其他格式来表示日期，比如`%F`代表完整的日期，`%b`代表缩写的月份等。

此外，我们还可以使用`strftime`命令来更灵活地转换日期。比如，我们可以将日期转换成英文形式或者其他语言的形式。下面是一个示例代码及输出：

```
Fish Shell Code:
string replace $current_date %F (strftime -f "%b %d, %Y" $current_date)

Output:
Nov 26, 2019
```

在上面的代码中，我们使用了`strftime`命令来将日期转换成英文形式。第一个参数是要转换的格式，第二个参数是要转换的日期变量。你可以根据自己的需要来灵活使用`strftime`命令。

## 参考链接

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Unix Date Format Cheat Sheet](https://www.computerhope.com/unix/udatefor.htm)