---
title:                "字符串插值"
aliases:
- /zh/bash/interpolating-a-string/
date:                  2024-01-20T17:50:05.555270-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串插值"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/interpolating-a-string.md"
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
