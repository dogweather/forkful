---
date: 2024-01-20 17:46:54.061883-07:00
description: "How to: (\u600E\u4E48\u505A:) \u5728\u5386\u53F2\u4E0A\uFF0C`expr length\
  \ \"$string\"`\u547D\u4EE4\u66FE\u7528\u4E8E\u65E9\u671F\u7684Shell\u811A\u672C\u4E2D\
  \u83B7\u53D6\u5B57\u7B26\u4E32\u957F\u5EA6\u3002\u73B0\u5728\uFF0C\u66F4\u4F18\u96C5\
  \u7684\u65B9\u6CD5\u662F`${#string}`\uFF0C\u8FD9\u4ECEBash\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.158447-06:00'
model: gpt-4-1106-preview
summary: "(\u600E\u4E48\u505A:) \u5728\u5386\u53F2\u4E0A\uFF0C`expr length \"$string\"\
  `\u547D\u4EE4\u66FE\u7528\u4E8E\u65E9\u671F\u7684Shell\u811A\u672C\u4E2D\u83B7\u53D6\
  \u5B57\u7B26\u4E32\u957F\u5EA6\u3002\u73B0\u5728\uFF0C\u66F4\u4F18\u96C5\u7684\u65B9\
  \u6CD5\u662F`${#string}`\uFF0C\u8FD9\u4ECEBash 3.0\u7248\u672C\u5F00\u59CB\u6210\
  \u4E3A\u6807\u51C6\u7528\u6CD5\u3002\u82E5\u5904\u7406\u591A\u5B57\u8282\u5B57\u7B26\
  \u96C6\uFF0C\u4F7F\u7528`mb_strlen`\u529F\u80FD\u53EF\u80FD\u66F4\u5408\u9002\uFF0C\
  \u4F46\u5728\u666E\u901A\u7684Bash\u811A\u672C\u4E2D\uFF0C`${#string}`\u901A\u5E38\
  \u5DF2\u7ECF\u8DB3\u591F\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

## How to: (怎么做:)
使用Bash获取字符串长度的简单例子：

```Bash
string="Hello, World!"
length=${#string}
echo $length
```

输出示例：

```
13
```

## Deep Dive (深入探讨)
在历史上，`expr length "$string"`命令曾用于早期的Shell脚本中获取字符串长度。现在，更优雅的方法是`${#string}`，这从Bash 3.0版本开始成为标准用法。若处理多字节字符集，使用`mb_strlen`功能可能更合适，但在普通的Bash脚本中，`${#string}`通常已经足够。

无需复杂的命令，Bash的内置功能提供了快速而有效的方式来确定字符串的长度。它直接计算字符串中的字符数，而不会因为涉及额外的命令或子shell而降低效率。

除了长度计算，`expr`和`awk`等工具提供了字符串处理的额外选项，但对于纯粹的长度测量来说，使用`${#string}`是最直接和高效的。

## See Also (另请参阅)
- Bash 手册: https://www.gnu.org/software/bash/manual/
- 高级Bash脚本编程指南: https://tldp.org/LDP/abs/html/
- 维基教科书上的Bash编程: https://en.wikibooks.org/wiki/Bash_Shell_Scripting
