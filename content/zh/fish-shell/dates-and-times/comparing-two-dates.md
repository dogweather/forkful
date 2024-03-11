---
date: 2024-01-20 17:32:51.283930-07:00
description: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F\u5C31\u662F\u68C0\u67E5\u5B83\u4EEC\
  \u8C01\u65E9\u8C01\u665A\u6216\u662F\u5426\u76F8\u540C\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u7BA1\u7406\u65E5\u5FD7\u3001\u8BC1\u4E66\
  \u6709\u6548\u6027\u3001\u4EFB\u52A1\u8C03\u5EA6\u7B49\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:22.087856-06:00'
model: gpt-4-1106-preview
summary: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F\u5C31\u662F\u68C0\u67E5\u5B83\u4EEC\
  \u8C01\u65E9\u8C01\u665A\u6216\u662F\u5426\u76F8\u540C\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u7BA1\u7406\u65E5\u5FD7\u3001\u8BC1\u4E66\
  \u6709\u6548\u6027\u3001\u4EFB\u52A1\u8C03\u5EA6\u7B49\u3002"
title: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？

比较两个日期就是检查它们谁早谁晚或是否相同。程序员这么做通常是为了管理日志、证书有效性、任务调度等。

## How to: 如何操作

```Fish Shell
# 设置日期
set date1 (date -d '2022-03-14' +%s) 
set date2 (date -d '2023-03-14' +%s) 

# 比较日期
if test $date1 -lt $date2
    echo "date1 is earlier than date2"
else if test $date1 -eq $date2
    echo "date1 is the same as date2"
else
    echo "date1 is later than date2"
end
```

输出样例：

```
date1 is earlier than date2
```

## Deep Dive 深度剖析

在早期的Shell编程中，比较日期并不直观。Unix timestamp（1970年1月1日以来的秒数）给我们提供了一种方法，可以轻松比较两个日期，因为每一个时间点都对应唯一的数字。`date +%s` 命令可以转换日期为 Unix timestamp。

Fish Shell与传统Bash有所不同，在语法和内建命令方面更加现代化，但比较日期的逻辑保持一致。使用`date`命令配合Fish Shell的`test`内建命令，就可以直观地比较日期。

万一Fish Shell不满足要求，你还可以考虑其他Shell脚本语言，比如Bash或者Zsh。或者对于更复杂的日期逻辑，可能需要脚本外部的工具或者编程语言，如Python的`datetime`模块。

## See Also 参考链接

- Fish Shell官方文档: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Unix Timestamp转换工具: [https://www.unixtimestamp.com/](https://www.unixtimestamp.com/)
- GNU Coreutils `date`说明: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
