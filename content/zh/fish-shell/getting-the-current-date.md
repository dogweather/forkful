---
title:    "Fish Shell: 获取当前日期"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么要获取当前日期

在编程中，有时需要获取当前日期来记录特定事件发生的时间，或者生成带有日期的文件名称。Fish Shell提供了一种简单而有效的方法来获取当前日期，让我们来看看如何实现。

## 如何操作

要获取当前日期，我们可以使用Fish Shell中的`date`命令，并将其赋值给一个变量。例如，我们可以将当前日期赋值给名为`today`的变量，并通过`echo`命令打印出来。

```Fish Shell
set today (date +"%Y-%m-%d")
echo $today
```

运行后，我们可以看到当前日期以`年-月-日`的格式打印出来。当然，我们也可以根据自己的需要自定义日期格式，比如`日期-月份-年`或`周几-日期`等等。只需要在`date`命令后的引号中改变格式即可。

## 深入了解

在了解如何获取当前日期的基本操作后，让我们进一步深入探讨一下。Fish Shell中的`date`命令实际上是来自于文本格式化工具包GNU coreutils，它有着更丰富的功能。通过`man date`命令我们可以查看它的所有选项和使用方法，可以发现`date`命令还可以用来设置其他日期，比如过去或未来的日期。

另外值得注意的是，Fish Shell提供了一些内置的变量来获取不同格式的日期，比如`$date`可以获取年月日时分秒，`$month`可以获取月份，`$day`可以获取日期，`$year`可以获取年份等等。这些内置变量可以在编程中更方便地使用，所以在编写代码时可以根据实际需求考虑使用哪一个。

### 另外几种获取当前日期的方法

除了使用`date`命令外，我们也可以通过Fish Shell中的其他命令来获取当前日期。比如`set today (echo $fish_date)`可以获取以`月/日/年`的格式表示的日期，`set today (date | awk '{print $2" "$3}')`可以获取以`月份 缩写的日期`的格式表示的日期等等。这些方法的具体实现可以根据自己的喜好选择。

## 查看也可

- [Fish Shell官方文档](https://fishshell.com/docs/current/index.html)
- [GNU coreutils官方文档](https://www.gnu.org/software/coreutils/manual/html_node/Date-conversion-specifiers.html)
- [Fish Shell For Mandarin](https://github.com/fish-shell/fish-shell/wiki/Documentation-Translation-Project)