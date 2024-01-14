---
title:    "Bash: 获取当前日期"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## 为什么使用Bash编程

Bash是一个流行的命令行语言，它可以在Linux和Mac系统上执行各种任务。其中一个有用的功能是获取当前日期，这在处理时间相关的任务或记录数据时非常有用。

## 如何使用Bash获取当前日期

```Bash
date +%Y%m%d
```

输出：

```
20210220
```

这段代码使用"date"命令获取当前日期，并通过"％Y"代表四位数的年份，"％m"代表两位数的月份，"％d"代表两位数的日期。你也可以使用其他格式，例如"％Y年％m月％d日"。

## 深入了解获取当前日期

更进一步，你也可以使用"％A"来表示星期几，"％B"来表示月份的完整名称，或者"％j"来表示一年中的第几天。

另外，你还可以使用"man date"命令来了解更多关于date命令的用法和选项。

## 参考链接

- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/)
- [Linux Man Pages](https://www.man7.org/linux/man-pages/)
- [使用Bash编程先导](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/)