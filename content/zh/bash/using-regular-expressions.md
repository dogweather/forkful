---
title:                "使用正则表达式"
date:                  2024-01-19
simple_title:         "使用正则表达式"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
正则表达式是文本搜索、替换和格式化的强大工具。程序员使用它进行复杂的文本操作，快速而准确。

## How to (如何操作？)
### 搜索文本
```Bash
echo "找找 'bash' 在不在这句话里。" | grep -Eo 'bash'
```
输出：
```
bash
```

### 文件中替换字符串
```Bash
echo -e "老王\n老李\n老赵" | sed 's/老/小/g'
```
输出：
```
小王
小李
小赵
```

### 校验格式
```Bash
[[ "example@domain.com" =~ ^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}$ ]] && echo "有效的邮箱地址" || echo "无效的邮箱地址"
```
输出：
```
有效的邮箱地址
```

## Deep Dive (深入了解)
正则表达式起源于1950年代的理论计算机科学。与通配符不同，它允许非常特定的字符串搜索模式。在各种编程语言和文本处理工具中有实现，但有细微差别。除了标准的Unix工具如grep和sed，现代编程语言如Python、Perl和Java均内建有强大的正则表达式库。

## See Also (另请参阅)
- [GNU Bash 文档](https://www.gnu.org/software/bash/manual/bash.html)
- [grep 手册页面](https://www.gnu.org/software/grep/manual/grep.html)
- [sed 手册页面](https://www.gnu.org/software/sed/manual/sed.html)
- [正则表达式教程](https://www.regular-expressions.info/tutorial.html)
