---
date: 2024-01-20 17:50:05.555270-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C) \u5728Bash\u4E2D\uFF0C\u4F7F\u7528\
  `$`\u7B26\u53F7\u548C\u5927\u62EC\u53F7`{}`\u8FDB\u884C\u5B57\u7B26\u4E32\u63D2\u503C\
  \u3002\u770B\u770B\u4E0B\u9762\u7684\u4F8B\u5B50\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.247300-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C) \u5728Bash\u4E2D\uFF0C\u4F7F\u7528`$`\u7B26\u53F7\
  \u548C\u5927\u62EC\u53F7`{}`\u8FDB\u884C\u5B57\u7B26\u4E32\u63D2\u503C\u3002\u770B\
  \u770B\u4E0B\u9762\u7684\u4F8B\u5B50\uFF1A."
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

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
