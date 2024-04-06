---
date: 2024-01-20 17:30:44.009715-07:00
description: "\u600E\u4E48\u505A\uFF1A \u8BA1\u7B97\u65E5\u671F\u6709\u7740\u60A0\u4E45\
  \u7684\u5386\u53F2\uFF0C\u4F46\u5728\u8BA1\u7B97\u673A\u7F16\u7A0B\u4E2D\uFF0C\u8FD9\
  \u662F\u901A\u8FC7\u7F16\u7A0B\u8BED\u8A00\u6216\u8005\u7CFB\u7EDF\u5DE5\u5177\u5B9E\
  \u73B0\u7684\u3002Fish Shell \u4F7F\u7528\u4E86 `date` \u547D\u4EE4\u6765\u6267\u884C\
  \u65E5\u671F\u8BA1\u7B97\uFF0C\u8FD9\u5B9E\u9645\u4E0A\u662F\u5BF9\u7CFB\u7EDF `date`\
  \ \u8C03\u7528\u7684\u5C01\u88C5\u3002\u5176\u4ED6\u8BED\u8A00\uFF0C\u5982 Python,\
  \ Ruby \u6216 JavaScript\uFF0C\u90FD\u6709\u81EA\u5DF1\u7684\u65E5\u671F\u5904\u7406\
  \u5E93\u3002 \u5728 Fish\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.556661-06:00'
model: gpt-4-1106-preview
summary: "\u5728 Fish Shell \u4E2D\uFF0C\u4F60\u53EF\u4EE5\u5229\u7528 `date` \u547D\
  \u4EE4\u975E\u5E38\u7075\u6D3B\u5730\u8BA1\u7B97\u65E5\u671F\u3002\u4FEE\u6539\u547D\
  \u4EE4\u4E2D\u7684 \"+3 days\" \u6216 \"-1 week\" \u5373\u53EF\u5B9A\u4E49\u4F60\
  \u60F3\u8981\u7684\u65F6\u95F4\u8DE8\u5EA6\u3002\u5B9E\u73B0\u7EC6\u8282\u4E0A\uFF0C\
  Fish Shell \u4F1A\u8C03\u7528\u7CFB\u7EDF\u7684 `date` \u5B9E\u73B0\uFF0C\u4F8B\u5982\
  \ GNU date \u6216 BSD date\uFF0C\u8FD9\u53D6\u51B3\u4E8E\u4F60\u7684\u64CD\u4F5C\
  \u7CFB\u7EDF."
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
weight: 26
---

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
