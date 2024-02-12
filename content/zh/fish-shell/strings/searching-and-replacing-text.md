---
title:                "搜索和替换文本"
aliases:
- /zh/fish-shell/searching-and-replacing-text.md
date:                  2024-01-20T17:58:00.634899-07:00
model:                 gpt-4-1106-preview
simple_title:         "搜索和替换文本"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么？)
搜索和替换文本就是找出指定的字符串并用另一个字符串替换掉它。程序员这么做是为了快速修正错误、更新数据或优化代码。

## How to: (如何操作：)
```Fish Shell
# 查找 "hello" 并替换为 "hi"
echo "hello world" | string replace "hello" "hi"
# 输出: hi world

# 全局替换，所有的 "o" 都换成 "0"
echo "lorem ipsum dolor sit amet" | string replace -a "o" "0"
# 输出: l0rem ipsum d0l0r sit amet
```

## Deep Dive (深入了解)
Fish Shell 提供内建的 `string` 命令来处理文本操作，其中包括搜索和替换功能。比起传统的 `sed` 命令，`string` 更直观简洁。`string replace` 支持基本的替换操作和正则表达式，可以执行局部或全局替换。全局替换藉由添加 `-a` 参数实现，指定 `-i` 可进行不区分大小写的替换。而历史上，UNIX 系统中经常借助 `sed` 或 `awk` 实现搜索替换，而现在 Fish Shell 的 `string` 提供了一个现代的替代方案。

## See Also (另请参见)
- Fish Shell 官方文档关于 `string` 命令的部分：https://fishshell.com/docs/current/cmds/string.html
- POSIX 标准 `sed` 命令教程：https://www.gnu.org/software/sed/manual/sed.html
- 正则表达式入门：https://www.regular-expressions.info/tutorial.html
