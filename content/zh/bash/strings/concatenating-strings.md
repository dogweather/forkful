---
date: 2024-01-20 17:34:09.588915-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u65E9\u671F\u7684 Unix \u7CFB\
  \u7EDF\u4E2D\u5C31\u652F\u6301\u5B57\u7B26\u4E32\u64CD\u4F5C\uFF0C\u5305\u62EC\u7B80\
  \u5355\u7684\u4E32\u8054\u3002\u4E00\u822C\u6765\u8BF4\uFF0C\u5B57\u7B26\u4E32\u4E32\
  \u8054\u6CA1\u6709\u4E13\u95E8\u7684\u64CD\u4F5C\u7B26\uFF1B\u53EA\u9700\u8981\u5C06\
  \u5B57\u7B26\u4E32\u7D27\u6328\u7740\u653E\u7F6E\u5373\u53EF\u3002\u6709\u4E9B\u8BED\
  \u8A00\u63D0\u4F9B\u4E86\u4E13\u95E8\u7684\u4E32\u8054\u51FD\u6570\uFF0C\u4F46\u5728\
  \ Bash \u4E2D\uFF0C\u8FD9\u662F\u901A\u8FC7\u53D8\u91CF\u5C55\u5F00\u5B9E\u73B0\u7684\
  \u3002 \u9664\u4E86\u76F4\u63A5\u5C55\u5F00\u53D8\u91CF\uFF0C\u8FD8\u6709\u4E9B\u522B\
  \u7684\u65B9\u6CD5\uFF1A - \u4F7F\u7528 `printf`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.107327-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u65E9\u671F\u7684 Unix \u7CFB\u7EDF\u4E2D\
  \u5C31\u652F\u6301\u5B57\u7B26\u4E32\u64CD\u4F5C\uFF0C\u5305\u62EC\u7B80\u5355\u7684\
  \u4E32\u8054\u3002\u4E00\u822C\u6765\u8BF4\uFF0C\u5B57\u7B26\u4E32\u4E32\u8054\u6CA1\
  \u6709\u4E13\u95E8\u7684\u64CD\u4F5C\u7B26\uFF1B\u53EA\u9700\u8981\u5C06\u5B57\u7B26\
  \u4E32\u7D27\u6328\u7740\u653E\u7F6E\u5373\u53EF\u3002\u6709\u4E9B\u8BED\u8A00\u63D0\
  \u4F9B\u4E86\u4E13\u95E8\u7684\u4E32\u8054\u51FD\u6570\uFF0C\u4F46\u5728 Bash \u4E2D\
  \uFF0C\u8FD9\u662F\u901A\u8FC7\u53D8\u91CF\u5C55\u5F00\u5B9E\u73B0\u7684\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

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
