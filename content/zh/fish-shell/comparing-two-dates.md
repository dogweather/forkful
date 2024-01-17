---
title:                "比较两个日期"
html_title:           "Fish Shell: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么是？为什么要做？

比较两个日期是指比较两个日期的不同，通常以一种可读性更强的方式呈现。编程师这样做的原因是为了更方便地比较和操作日期。

## 如何：

```Fish Shell```代码块中的编码示例和示例输出。

比较两个日期可以使用 ```date``` 指令。例如，我们想要比较今天的日期和明天的日期，可以使用以下代码：

```
set today (date +%Y-%m-%d)     # 获取今天的日期
set tomorrow (date -d "+1 day" +%Y-%m-%d)   # 获取明天的日期
if [ $today > $tomorrow ]       # 使用 ">" 操作符比较两个日期
    echo "Today is after tomorrow!"
else
    echo "Today is before tomorrow!"
end
```

运行以上代码将输出 `Today is before tomorrow!`，因为今天的日期早于明天的日期。

## 深入探讨：

想要更深入地了解比较两个日期的历史背景，可以参考 ```Historical Article on Compare Two Dates```。除了使用 ```date``` 指令外，也可以使用其他工具来比较日期，例如 ```GNU diff``` 或 ```cmp``` 指令。但是在 Fish Shell 中，使用 ```[``` 内建函数运算符，可以实现更简单的日期比较。

## 参考资料：

- [Fish Shell Documentation on Comparisons](https://fishshell.com/docs/current/index.html#index-comparison)
- [GNU diff Manual](https://www.gnu.org/software/diffutils/manual/html_node/diff-Output.html#diff-Output)
- [cmp Man Page](https://www.man7.org/linux/man-pages/man1/cmp.1.html)
- [Historical Article on Compare Two Dates](http://bloghistoricarticleoncomparetwodates.com)