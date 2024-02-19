---
aliases:
- /zh/bash/finding-the-length-of-a-string/
date: 2024-01-20 17:46:54.061883-07:00
description: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6\u5C31\u662F\u627E\u51FA\
  \u5B83\u5305\u542B\u591A\u5C11\u4E2A\u5B57\u7B26\u3002\u7A0B\u5E8F\u5458\u901A\u5E38\
  \u9700\u8981\u8FD9\u4E2A\u4FE1\u606F\u6765\u9A8C\u8BC1\u6570\u636E\u3001\u622A\u53D6\
  \u5B57\u7B26\u4E32\u6216\u7B80\u5355\u5730\u7EDF\u8BA1\u4FE1\u606F\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.282685
model: gpt-4-1106-preview
summary: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6\u5C31\u662F\u627E\u51FA\
  \u5B83\u5305\u542B\u591A\u5C11\u4E2A\u5B57\u7B26\u3002\u7A0B\u5E8F\u5458\u901A\u5E38\
  \u9700\u8981\u8FD9\u4E2A\u4FE1\u606F\u6765\u9A8C\u8BC1\u6570\u636E\u3001\u622A\u53D6\
  \u5B57\u7B26\u4E32\u6216\u7B80\u5355\u5730\u7EDF\u8BA1\u4FE1\u606F\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么?)
获取字符串的长度就是找出它包含多少个字符。程序员通常需要这个信息来验证数据、截取字符串或简单地统计信息。

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
