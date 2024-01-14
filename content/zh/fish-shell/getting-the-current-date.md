---
title:                "Fish Shell: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么：获取当前日期的理由

获取当前日期对程序员来说是一项非常基本的任务，因为它可以帮助我们记录和跟踪程序运行的时间，也可以作为程序中的重要参考点。使用Fish Shell可以轻松地获取当前日期，并且操作简单高效，让我们一起来看看怎么做吧！

## 如何操作：Fish Shell代码示例

```Fish Shell
# 使用"date"命令来获取当前日期
date
# 获取带有年月日时分秒的详细日期
date +"%Y-%m-%d %H:%M:%S"
# 获取带有星期的日期
date +"%A, %d %b %Y"
```

以上代码将会输出以下结果：

```
Mon Oct 11 17:24:25 CST 2021
2021-10-11 17:24:25
Monday, 11 Oct 2021
```

通过使用不同的格式可以获取不同类型的日期，比如只需要年份或者月份等，可以根据自己的需求来自定义表达式。

## 深入了解：获取当前日期的更多信息

Fish Shell 中，默认使用系统的 date 命令来获取日期，也可以使用其它 Linux / Unix 系统提供的同名命令，比如 FreeBSD 上有个 coreutils 家族软件中的 date 命令，可以使用  coreutils-date 来调用。另外，文档建议不要使用 isodate 来格式化 日期，因为它会不易倒置，建议使用 Fish Shell 内置的 datef 命令来格式化输出，比如：```datef %02m-%02d``` ，即日月分，缺乏年份和连字符，当然，你也可以根据自己的习惯来调整筛选和调整日期格式。

## 参考资料

- [Fish Shell官方文档](https://fishshell.com/docs/current/)
- [Linux / Unix 系统命令文档](https://www.freebsd.org/man.cgi?query=date&sektion=1&arch=default&manpath=FreeBSD+12.2-RELEASE+and+Ports)
- [Fish Shell格式化输出指导](https://fishshell.com/docs/current/cmds/date.html)
- [Linux不同发行版的常用命令对比](https://www.tecmint.com/linux-date-command-examples/)