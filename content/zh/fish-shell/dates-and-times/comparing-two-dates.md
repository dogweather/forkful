---
date: 2024-01-20 17:32:51.283930-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C \u5728\u65E9\u671F\u7684Shell\u7F16\
  \u7A0B\u4E2D\uFF0C\u6BD4\u8F83\u65E5\u671F\u5E76\u4E0D\u76F4\u89C2\u3002Unix timestamp\uFF08\
  1970\u5E741\u67081\u65E5\u4EE5\u6765\u7684\u79D2\u6570\uFF09\u7ED9\u6211\u4EEC\u63D0\
  \u4F9B\u4E86\u4E00\u79CD\u65B9\u6CD5\uFF0C\u53EF\u4EE5\u8F7B\u677E\u6BD4\u8F83\u4E24\
  \u4E2A\u65E5\u671F\uFF0C\u56E0\u4E3A\u6BCF\u4E00\u4E2A\u65F6\u95F4\u70B9\u90FD\u5BF9\
  \u5E94\u552F\u4E00\u7684\u6570\u5B57\u3002`date +%s` \u547D\u4EE4\u53EF\u4EE5\u8F6C\
  \u6362\u65E5\u671F\u4E3A Unix timestamp\u3002 Fish\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.555574-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C \u5728\u65E9\u671F\u7684Shell\u7F16\u7A0B\u4E2D\
  \uFF0C\u6BD4\u8F83\u65E5\u671F\u5E76\u4E0D\u76F4\u89C2\u3002Unix timestamp\uFF08\
  1970\u5E741\u67081\u65E5\u4EE5\u6765\u7684\u79D2\u6570\uFF09\u7ED9\u6211\u4EEC\u63D0\
  \u4F9B\u4E86\u4E00\u79CD\u65B9\u6CD5\uFF0C\u53EF\u4EE5\u8F7B\u677E\u6BD4\u8F83\u4E24\
  \u4E2A\u65E5\u671F\uFF0C\u56E0\u4E3A\u6BCF\u4E00\u4E2A\u65F6\u95F4\u70B9\u90FD\u5BF9\
  \u5E94\u552F\u4E00\u7684\u6570\u5B57\u3002`date +%s` \u547D\u4EE4\u53EF\u4EE5\u8F6C\
  \u6362\u65E5\u671F\u4E3A Unix timestamp."
title: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F"
weight: 27
---

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
