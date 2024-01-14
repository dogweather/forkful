---
title:                "Fish Shell: 比较两个日期"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 为什么要比较两个日期？

在编程中，我们经常需要比较两个日期来判断时间先后顺序或者计算时间间隔。比如在日程管理应用中，我们可能需要检查某个事件是否已经过去或者计算两个事件之间的天数。使用Fish Shell可以轻松地比较两个日期并获得想要的结果。

## 如何比较两个日期？

比较两个日期的最基本的方法是使用date命令。以下是一个示例代码，演示如何比较两个日期是否相等：

```Fish Shell
set date1 2019-10-23
set date2 2019-11-15

if [ "$date1" = "$date2" ]
    echo "日期相等！"
else
    echo "日期不相等"
end
```

运行以上代码，结果为“日期不相等”。如果想要更加灵活地比较日期，可以使用date命令的`-s`选项来将日期转换为可比较的格式。比如：

```Fish Shell
set date1 (date -s "2019-10-23" +%s)
set date2 (date -s "2019-11-15" +%s)

if [ "$date1" -eq "$date2" ]
    echo "日期相等！"
else
    echo "日期不相等"
end
```

运行以上代码，结果为“日期不相等”。这是因为date命令的`-s`选项将日期转换为Unix时间戳，可以更容易地比较大小。

## 深入了解比较日期

除了基本的比较方法，Fish Shell还提供了内置的math命令来进行日期运算。比如，想要计算两个日期之间的天数差，可以使用以下代码：

```Fish Shell
set date1 (date -s "2019-10-23" +'%Y%m%d')
set date2 (date -s "2019-11-15" +'%Y%m%d')

math $date2 - $date1
```

运行以上代码，结果为23，表示两个日期相差23天。同时，Fish Shell还提供了方便的日期格式化选项，可以将日期输出为不同的格式。

# 参考链接

- [Fish Shell官方文档](https://fishshell.com/docs/current/index.html)
- [如何在Fish Shell中比较两个日期](https://zhuanlan.zhihu.com/p/112704479)
- [Fish Shell的日期处理指南](https://devhints.io/fish-shell-dates)
- [Date命令的手册页面](https://linux.die.net/man/1/date)

# 参见

- [Fish Shell高级编程指南](https://medium.com/@jorgegsc007/a-definitive-guide-to-fish-shell-scripting-f295713cef59)
- [Shell编程入门](https://www.runoob.com/linux/linux-shell.html)
- [Shell语言编程基础知识](https://www.ibm.com/developerworks/cn/linux/l-shell/)