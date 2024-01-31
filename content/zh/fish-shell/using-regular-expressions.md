---
title:                "使用正则表达式"
date:                  2024-01-19
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (什么是正则表达式及其用途?)
正则表达式是字符串的搜索和替换模式。程序员使用它们进行复杂的文本处理，节省时间和提高效率。

## How to: (如何使用：)
在Fish Shell中，你可以使用`string`命令配合正则表达式进行文本操作。

```fish
# 查找匹配
echo "fish_shell_rocks" | string match -r "sh.*rocks"

# 输出：sh_shell_rocks

# 替换匹配
echo "fish_shell_rocks" | string replace -r "sh.*rocks" "is_awesome"

# 输出：fish_is_awesome
```

## Deep Dive (深入了解)
正则表达式起源于20世纪50年代的理论计算机科学。Perl和Python等语言内建了强大的正则表达式工具。Fish Shell使用了自己的`string`命令提供正则表达式支持，它执行速度快且易于理解。虽然性能不及专业工具如`grep`，但在大多日常场景下足够用。

## See Also (参考链接)
- Fish Shell官方文档：[string](https://fishshell.com/docs/current/cmds/string.html)
- 正则表达式练习：[Regexr](https://regexr.com/)
- 正则表达式学习资源：[Regular-Expressions.info](https://www.regular-expressions.info/)
