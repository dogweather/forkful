---
title:                "Fish Shell: 获取当前日期"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

**为什么：**获取当前日期有什么好处？

获取当前日期是一项非常基本的编程任务，它可以让我们在程序中使用准确的时间信息，从而使我们的程序更加强大和实用。比如，我们可以通过获取当前日期来创建日志文件，记录程序运行的时间，或者为用户提供有用的提示信息。

## 如何使用Fish Shell获取当前日期

获取当前日期在Fish Shell中非常简单，我们只需要使用内置的`date`命令即可。以下是一个简单的示例代码，展示如何使用Fish Shell来获取当前日期：

```Fish Shell
date
```

运行以上代码，我们可以得到如下输出：

```
Wed Sep 29 22:08:42 CST 2021
```

在这个输出中，我们可以看到当前的日期以及精确到秒的时间信息。如果我们只想获取日期或者时间的某一部分，我们可以使用`+%<format>`选项来指定输出格式。以下是一些常用的格式：

- `%Y`：四位数的年份
- `%m`：两位数的月份
- `%d`：两位数的日期
- `%H`：24小时制的小时数
- `%M`：两位数的分钟数
- `%S`：两位数的秒数

我们可以将以上格式组合起来，来得到我们需要的精确时间信息。比如，如果我们只想获取当前日期的年份和月份，我们可以使用以下命令：

```Fish Shell
date +'%Y-%m'
```

输出结果为`2021-09`。

## 深入了解获取当前日期

除了常用的`date`命令外，Fish Shell还提供了一些其他的命令来获取当前日期和时间相关的信息。比如，我们可以使用`clock`命令来获取当前的UNIX时间戳，以及计算机的启动时间和时区。还可以使用`cal`命令来显示当前的月历。

在深入了解这些命令的更多特性之前，我们建议您先花一些时间阅读Fish Shell的官方文档，以便更好地理解和利用这些命令。

## 参考资料

- [Fish Shell官方文档](https://fishshell.com/docs/current/)
- [Unix & Linux Stack Exchange：获取当前日期和时间的命令](https://unix.stackexchange.com/questions/67822/how-to-get-current-date-time-in-fish-bottom-shell)
- [学习Fish Shell：获取当前日期和时间](https://fishshell.com/docs/current/tutorial.html#tut_dates)