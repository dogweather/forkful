---
date: 2024-01-20 17:50:05.555270-07:00
description: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u628A\u53D8\u91CF\u503C\u5D4C\u5165\
  \u5B57\u7B26\u4E32\u4E2D\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u662F\u4E3A\u4E86\u52A8\u6001\u6784\u5EFA\u5B57\u7B26\u4E32\uFF0C\u63D0\u9AD8\u4EE3\
  \u7801\u7684\u7075\u6D3B\u6027\u548C\u53EF\u8BFB\u6027\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.511827-07:00'
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u628A\u53D8\u91CF\u503C\u5D4C\u5165\
  \u5B57\u7B26\u4E32\u4E2D\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u662F\u4E3A\u4E86\u52A8\u6001\u6784\u5EFA\u5B57\u7B26\u4E32\uFF0C\u63D0\u9AD8\u4EE3\
  \u7801\u7684\u7075\u6D3B\u6027\u548C\u53EF\u8BFB\u6027\u3002"
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么?)
字符串插值是把变量值嵌入字符串中的过程。程序员这么做是为了动态构建字符串，提高代码的灵活性和可读性。

## How to: (如何操作)
在Bash中，使用`$`符号和大括号`{}`进行字符串插值。看看下面的例子：

```Bash
name="世界"
greeting="你好, ${name}!"
echo $greeting
```

输出将是：

```
你好, 世界!
```

## Deep Dive (深入了解)
字符串插值在Shell脚本中极为常见。它由历史悠久的Unix Shell演变而来。不用插值，你得硬编码文本和变量，不够灵活。

除了`${}`，还有旧式的反引号`` ` ``，也可以用于执行命令并插入结果，不过建议使用`$()`。

在实现细节上，插值发生在Bash对命令行进行的Word Splitting步骤之前，确保字符串被作为一个整体处理。

## See Also (另见)
- Bash官方文档: [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- Advanced Bash-Scripting Guide: [String Operations](http://tldp.org/LDP/abs/html/string-manipulation.html)
- 命令替换的详细解释: [Command Substitution](https://www.gnu.org/software/bash/manual/html_node/Command-Substitution.html)
