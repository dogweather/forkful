---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在计算机编程中，比较两个日期就是确定他们之间的早晚顺序或查看两者间隔了多久。程序员经常需要进行此操作以排列事件，设定提醒，或者测量过程运行的间隔时间。

## 如何实现：
在 Fish Shell 中，我们可以通过以下步骤比较两个日期：

```Fish Shell
# 创建两个日期变量
set date1 (date --date="2020-01-01" +%s)
set date2 (date --date="2020-01-05" +%s)

# 比较两个日期
if test "$date1" -gt "$date2"
    echo "2020-01-01 在 2020-01-05 之后"
else if test "$date1" -lt "$date2"
    echo "2020-01-01 在 2020-01-05 之前"
else
    echo "两个日期相同"
end
```
执行以上程序，输出结果将会是 "2020-01-01 在 2020-01-05 之前"。

## 深入了解：
Fish Shell是一种用户友好的shell，最初是在2005年由Axel Liljencrantz创立。比较日期在日常应用中非常常见，通常用于日历事件，提示以及监测程序运行时间。

在Fish Shell中，我们使用内置函数 `date` 以及 `if` 逻辑判断语句来比较两个日期。还可以使用其他脚本和程序语言（比如Python、JavaScript等）实现日期的比较，但Fish Shell以其直接性和简洁性而受到许多开发者的青睐。

比较两个日期的具体实现取决于你如何获取和存储日期数据。在Fish Shell中，我们通常使用标准日期形式 'YYYY-MM-DD'，并借助 `date` 函数将它们转化为时间戳格式（自1970年1月1日以来的秒数），然后进行比较。

## 更多参考：
- Fish Shell官方文档: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Linux `date` 命令: [https://www.cyberciti.biz/faq/unix-linux-get-time-in-seconds-since-the-epoch-jan-1970/](https://www.cyberciti.biz/faq/unix-linux-get-time-in-seconds-since-the-epoch-jan-1970/)
- Fish Shell比较操作: [https://fishshell.com/docs/current/cmds/test.html](https://fishshell.com/docs/current/cmds/test.html)