---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么和为什么？

子字符串的提取是从长字符序列中获取特定部分的过程。程序员会这样做以简化字符串处理任务和高效进行数据分析。

## 如何做：

在 Fish Shell 中，我们可以使用 `string sub` 命令来提取字符串：

```Fish Shell
> set str 'Hello, World!'
> string sub -s 8 -l 5 -- $str

World
```

在上述代码中，`-s` 参数定义了起始位置，'-l' 定义了长度，`--` 后面就是你的字符串。

## 深潜：

在历史上，许多 Shell （比如 Bash） 使用了类似 `${string:position:length}` 的语法来提取字符串。但是 Fish Shell 引入了 `string sub` 命令，使得这个过程变得更明确和易懂。

还有其他的选择，比如用 `sed` 或 `awk` 这样的工具，但是 `string sub` 命令在易用性和可读性上具有明显的优势。

为了更高效的执行，Fish Shell 采用了`Newlocale`做字符边界处理，这让不同的语言环境（比如在处理非ASCII字符）得以更精准的定位和提取。

## 另请参阅：

1. Fish Shell 官方文档，详细解释如何使用 `string sub` 提取子字符串：https://fishshell.com/docs/current/cmds/string.html
2. 关于字符串处理的一般性理论和实践，可以参考：https://en.wikipedia.org/wiki/String_operations

没有结束部分。