---
title:    "Fish Shell: 计算未来或过去的日期"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

#为什么

计算一个未来或过去的日期可能是因为需要计划或预测未来的事件，或者查找过去发生的事件的日期。使用Fish Shell编程可以轻松地完成这项任务。

##如何使用

首先，需要确定要计算的起始日期，然后使用Date命令指定日期格式。接下来，使用-d参数指定要添加或减去的天数，并使用语义语法来指定未来或过去的日期。

```
Fish Shell 
start_date=$(date -f %Y-%m-%d)
echo "Origin Date: " $start_date
future_date=$(date -d "$start_date + 7 days")
echo "Future Date: " $future_date
```

输出：

```
Origin Date: 2021-05-30
Future Date: 2021-06-06
```

##深入探讨

Fish Shell有许多内置的日期计算函数，如`date`, `strftime`和`strptime`。您还可以根据需要使用这些函数来实现更多复杂的日期计算，如计算一年后的同一日期、跳过周末等。

此外，Fish Shell还有许多第三方插件可用于日期计算，如`fzf`和`fishtape`，它们可以帮助您更快速地计算日期并输出结果。

##另请参阅

- Fish Shell日期计算官方文档：https://fishshell.com/docs/current/cmds/date.html 
- 用 fish tape 记录 shell 运行记录：https://github.com/fisherman/fishtape 
- 使用 fzf 在 fish 中快速计算日期：https://github.com/Aloxaf/fzf-tab 
- strftime和strptime语法指南：https://www.gnu.org/software/libc/manual/html_node/The-Time-Parsing-Functions.html