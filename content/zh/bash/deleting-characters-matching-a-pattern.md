---
title:                "匹配模式删除字符"
date:                  2024-01-20T17:41:33.102202-07:00
model:                 gpt-4-1106-preview
simple_title:         "匹配模式删除字符"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么？
在Bash中删除匹配模式的字符能帮助我们清理和格式化字符串。编程过程中做这个常是因为要处理输入或整理数据。

## How to: 如何实现：

用变量和模式替换来删除字符：

```Bash
# 删除所有数字
string="Year2023"
echo "${string//[^0-9]/}"

# 输出：2023

# 删除小写字母
string="HelloWorld"
echo "${string//[a-z]/}"

# 输出：HW
```

用`sed`进行模式匹配和删除：

```Bash
echo "Remove vowels from this sentence." | sed 's/[aeiou]//g'

# 输出：Rmv vwls frm ths sntnc.
```

## Deep Dive: 深入探究
过去，Unix shell 脚本主要用`tr`或`sed`做字符处理。Bash之后加入了内建的模式替换功能，提供更快捷的方法。

替换语法 `string/pattern/replacement` 里：
- `string` 是原始文本；
- `pattern` 是要匹配的模式；
- `replacement` 可以是空，这样模式匹配的部分就会被删除。

如果使用`sed`：
- `sed` 很强大，用于文本处理；
- `'s/[pattern]//g'` 命令将会全局删除匹配模式的字符。

## See Also: 相关链接
- Bash 手册: https://www.gnu.org/software/bash/manual/bash.html
- `sed` 入门: https://www.gnu.org/software/sed/manual/sed.html
- 高级 Bash 脚本编写指南: https://www.tldp.org/LDP/abs/html/
