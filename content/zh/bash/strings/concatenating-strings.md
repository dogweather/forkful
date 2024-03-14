---
date: 2024-01-20 17:34:09.588915-07:00
description: "\u4E32\u8054\u5B57\u7B26\u4E32\u5C31\u662F\u5C06\u591A\u4E2A\u5B57\u7B26\
  \u4E32\u8FDE\u63A5\u6210\u4E00\u4E2A\u5B57\u7B26\u4E32\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u6765\u5408\u5E76\u6587\u672C\uFF0C\u6784\u9020\u547D\u4EE4\u6216\u751F\
  \u6210\u8F93\u51FA\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.949394-06:00'
model: gpt-4-1106-preview
summary: "\u4E32\u8054\u5B57\u7B26\u4E32\u5C31\u662F\u5C06\u591A\u4E2A\u5B57\u7B26\
  \u4E32\u8FDE\u63A5\u6210\u4E00\u4E2A\u5B57\u7B26\u4E32\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u6765\u5408\u5E76\u6587\u672C\uFF0C\u6784\u9020\u547D\u4EE4\u6216\u751F\
  \u6210\u8F93\u51FA\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)

串联字符串就是将多个字符串连接成一个字符串。程序员这样做来合并文本，构造命令或生成输出。

## How to: (如何操作：)

```Bash
# Concatenating two strings
str1="Hello"
str2="World"
concatenated_str="$str1 $str2"
echo $concatenated_str # 输出 Hello World

# Appending string to a variable
str3="!"
concatenated_str+=$str3
echo $concatenated_str # 输出 Hello World!

# Using brace expansion for concatenation
echo "${str1}Beautiful ${str2}" # 输出 HelloBeautiful World
```

## Deep Dive (深入探索)

早期的 Unix 系统中就支持字符串操作，包括简单的串联。一般来说，字符串串联没有专门的操作符；只需要将字符串紧挨着放置即可。有些语言提供了专门的串联函数，但在 Bash 中，这是通过变量展开实现的。

除了直接展开变量，还有些别的方法：

- 使用 `printf` 命令串联字符串：`printf "%s%s\n" "$str1" "$str2"`
- 利用命令替换：`concatenated_str=$(echo "$str1$str2")`

每种方法都有它的用途，取决于具体场景和偏好。但对于 Bash 脚本，上面展示的变量展开通常是最直接、最清晰的方式。

## See Also (另请参阅)

- [Bash String Manipulation](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
- [Bash Reference Manual](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
