---
title:                "计算未来或过去的日期"
html_title:           "Fish Shell: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

计算未来或过去的日期，即根据给定日期确定将来或过去的某一天。程序员经常这么做以便于处理时间敏感的任务，并确保其准确性。

## 演示如何：

在 Fish Shell （当前版本）中，我们使用 `date` 命令和适当的选项。下面是几个示例：

```Fish Shell
# 计算三天后的日期
set future_date (date -v+3d "+%Y%m%d")

# 输出示例 ：20220403
echo $future_date

# 计算一周前的日期
set past_date (date -v-1w "+%Y%m%d")

# 输出示例： 20220326
echo $past_date
```

## 深入了解

1. 历史背景：计算未来或过去的日期这一功能源自于早期的 Unix 系统，后来被包含在了各种现代 shell 如 Fish Shell 中。
2. 替代方案：除了 `date` 命令，其他方法如使用 Python 或 Perl 中的日期函数库也可以实现同样的功能。
3. 实现细节：Fish Shell 通过解析 `date` 命令的参数来进行日期计算。例如，"+%Y%m%d" 格式的字符串就被解析成具体的日期。

## 另请参见

1. [Fish Shell 官网](https://fishshell.com)
2. [计算日期的更多方法](https://www.cyberciti.biz/faq/unix-linux-appleosx-bsd-shell-appending-date-to-filename/)
3. [详细的 date 命令用法](https://ss64.com/bash/date.html)