---
title:                "计算未来或过去的日期"
aliases:
- /zh/fish-shell/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:30:44.009715-07:00
model:                 gpt-4-1106-preview
simple_title:         "计算未来或过去的日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
计算未来或过去的日期就是确定一个日期在今天之前或之后的具体天数。程序员这么做是为了处理任务计划、计算年龄、提醒事件或有效期限等场合。

## 怎么做：
```Fish Shell
# 计算未来的日期
set -l future_date (date -d "+3 days" +"%Y-%m-%d")
echo $future_date
```
输出样例：
```
2023-03-17
```
```Fish Shell
# 计算过去的日期
set -l past_date (date -d "-1 week" +"%Y-%m-%d")
echo $past_date
```
输出样例：
```
2023-03-03
```

## 深入探索
计算日期有着悠久的历史，但在计算机编程中，这是通过编程语言或者系统工具实现的。Fish Shell 使用了 `date` 命令来执行日期计算，这实际上是对系统 `date` 调用的封装。其他语言，如 Python, Ruby 或 JavaScript，都有自己的日期处理库。

在 Fish Shell 中，你可以利用 `date` 命令非常灵活地计算日期。修改命令中的 "+3 days" 或 "-1 week" 即可定义你想要的时间跨度。实现细节上，Fish Shell 会调用系统的 `date` 实现，例如 GNU date 或 BSD date，这取决于你的操作系统。

除此之外，还可以用其他方法，例如重写 `strftime` 和 `strptime` 函数，或者使用其他命令行工具比如 `gdate`（在某些系统中是 GNU date 的名字）, 但 `date` 命令因其简单和广泛可用而常被首选。

## 参见
- Fish Shell 官方文档: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- `date` 命令详细用例: [https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
