---
date: 2024-01-20 17:30:44.009715-07:00
description: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u5C31\u662F\
  \u786E\u5B9A\u4E00\u4E2A\u65E5\u671F\u5728\u4ECA\u5929\u4E4B\u524D\u6216\u4E4B\u540E\
  \u7684\u5177\u4F53\u5929\u6570\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\
  \u4E86\u5904\u7406\u4EFB\u52A1\u8BA1\u5212\u3001\u8BA1\u7B97\u5E74\u9F84\u3001\u63D0\
  \u9192\u4E8B\u4EF6\u6216\u6709\u6548\u671F\u9650\u7B49\u573A\u5408\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.280615-06:00'
model: gpt-4-1106-preview
summary: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u5C31\u662F\
  \u786E\u5B9A\u4E00\u4E2A\u65E5\u671F\u5728\u4ECA\u5929\u4E4B\u524D\u6216\u4E4B\u540E\
  \u7684\u5177\u4F53\u5929\u6570\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\
  \u4E86\u5904\u7406\u4EFB\u52A1\u8BA1\u5212\u3001\u8BA1\u7B97\u5E74\u9F84\u3001\u63D0\
  \u9192\u4E8B\u4EF6\u6216\u6709\u6548\u671F\u9650\u7B49\u573A\u5408\u3002."
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
weight: 26
---

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
