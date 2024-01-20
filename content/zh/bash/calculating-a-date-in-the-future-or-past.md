---
title:                "计算未来或过去的日期"
html_title:           "Bash: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

---

## 什么 & 为什么？

日期计算是用来确定未来或过去的某一日期。程序员经常需要进行日期计算来处理时间戳，制定计划，或者做时间区间比较。 

---

## 如何做：

Bash 中我们可以使用 `date` 命令加上 `-d` 标记来轻松计算未来或过去的日期。下面是一些例子：

```Bash
# 计算三天后的日期
date -d "+3 days"

# 计算一周前的日期
date -d "-1 week"
```

运行这些命令后你会得到类似如下的输出：

```Bash
# 输出示例：
Fri Feb 17 11:14:32 PST 2023
Sat Feb 10 11:14:32 PST 2023
```

---

## 深入研究：

`date` 命令在计算日期时提供了极大的灵活性。它可以处理各种日期和时间的增减，如 "+5 weeks"、"-7 hours" 等等。

在历史上，还有一些其他的方法用于日期计算，包括复杂的算法和表。然而，这些方法都比 `date` 命令更为复杂，使用也不太方便。

你可能也会遇到在其他编程环境下进行日期计算的情况，比如 Python 里的 `datetime` 库或 JavaScript 里的 `Date` 对象。这些都是用来处理更复杂日期计算的工具。

你也可以使用一些高级编程语言作为 Bash 的替代品，如 Python、Perl、Ruby 等等。它们提供了更强大的日期计算能力，可以进行更复杂的日期操作，但个别情况下可能会过于复杂。

---

## 参见：

以下是一些有用的链接，供你进一步了解和学习 Bash 的日期计算：

- Bash 脚本如何将日期加减运算：https://www.cyberciti.biz/faq/howto-linux-unix-date-command-compare-script/.
- Date 命令：https://man7.org/linux/man-pages/man1/date.1.html.
- Python 的 datetime 库：https://docs.python.org/3/library/datetime.html.
- JavaScript 的 Date 对象：https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date.