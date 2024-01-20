---
title:                "获取当前日期"
date:                  2024-01-20T15:12:47.981037-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
获取当前日期让你知道现在是什么时候。程序员用它来记录事件、设置时间戳或检查时效性。

## How to: (如何操作：)
```Bash
# 获取当前日期并显示
date

# 输出样例：
# 星期二 三月  21 14:23:42 CST 2023

# 自定义格式输出
date +"%Y-%m-%d %H:%M:%S"

# 输出样例：
# 2023-03-21 14:23:42
```

## Deep Dive (深入了解)
Bash中`date`命令是从Unix时代继承下来的，用于显示和设置系统日期和时间。其他方法比如`hwclock`直接跟硬件通讯。`date`命令因其简单性和灵活性（可自定义输出格式）而受欢迎。在脚本里，正确使用日期时间数据是很重要的，比如创建日志文件名或进行日期计算。

## See Also (另见)
- GNU Coreutils: https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation
- Bash 手册: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html 
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/