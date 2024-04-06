---
date: 2024-01-20 17:35:49.349292-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.183938-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\
  \u4E32\u5728Unix\u548CLinux\u64CD\u4F5C\u7CFB\u7EDF\u7684\u5386\u53F2\u4E2D\u5360\
  \u636E\u7740\u91CD\u8981\u7684\u4F4D\u7F6E\uFF0C\u56E0\u4E3A\u8FD9\u4E9B\u64CD\u4F5C\
  \u7CFB\u7EDF\u7ECF\u5E38\u9700\u8981\u5C06\u65E5\u671F\u7528\u4E8E\u811A\u672C\u548C\
  \u65E5\u5FD7\u6587\u4EF6\u3002`date`\u547D\u4EE4\u81EAUnix\u8BDE\u751F\u4EE5\u6765\
  \u5C31\u5B58\u5728\uFF0C\u5B83\u63D0\u4F9B\u4E86\u591A\u79CD\u9009\u9879\u6765\u81EA\
  \u5B9A\u4E49\u65E5\u671F\u683C\u5F0F\u3002\u9664\u4E86`date`\u547D\u4EE4\uFF0CBash\u7A0B\
  \u5E8F\u5458\u8FD8\u53EF\u4EE5\u7528\u5176\u4ED6\u5DE5\u5177\u4F8B\u5982`awk`\u6216\
  `printf`\u8FDB\u884C\u65E5\u671F\u5B57\u7B26\u4E32\u5904\u7406\u3002\u5C06\u65E5\
  \u671F\u5B58\u50A8\u4E3A\u5B57\u7B26\u4E32\u65F6\uFF0C\u56FD\u9645\u6807\u51C6ISO\
  \ 8601\u683C\u5F0F\uFF08YYYY-MM-DD\uFF09\u662F\u9996\u9009\uFF0C\u56E0\u4E3A\u5B83\
  \u907F\u514D\u4E86\u533A\u57DF\u5DEE\u5F02\u548C\u6DF7\u6DC6\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## How to: (如何操作：)
```Bash
# 获取当前日期并转换为字符串格式，例如：2023-04-12
current_date=$(date '+%Y-%m-%d')
echo "Today's date in string is: $current_date"
```
输出样例：
```
Today's date in string is: 2023-04-12
```

## Deep Dive (深入了解)
日期转换为字符串在Unix和Linux操作系统的历史中占据着重要的位置，因为这些操作系统经常需要将日期用于脚本和日志文件。`date`命令自Unix诞生以来就存在，它提供了多种选项来自定义日期格式。除了`date`命令，Bash程序员还可以用其他工具例如`awk`或`printf`进行日期字符串处理。将日期存储为字符串时，国际标准ISO 8601格式（YYYY-MM-DD）是首选，因为它避免了区域差异和混淆。

## See Also (另请参阅)
- GNU Coreutils `date`命令手册: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- POSIX标准：https://pubs.opengroup.org/onlinepubs/9699919799/utilities/date.html
- Bash编程入门教程：https://tldp.org/LDP/Bash-Beginners-Guide/html/
