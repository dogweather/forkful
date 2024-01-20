---
title:                "搜索和替换文本"
html_title:           "Kotlin: 搜索和替换文本"
simple_title:         "搜索和替换文本"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 在 Bash 中搜索和替换文本的方法

## 什么以及为什么?

搜索和替换文本是找出并修改现有的文本构造的方法。程序员这么做是为了更新代码、设置或者数据。

## 如何做:

在 Bash 中，你可以用 `sed` 命令来搜索和替换文本。例如:

```Bash
echo "大家好,我是小红" | sed 's/小红/小明/'
```
当你运行这行代码，你将得到 “大家好，我是小明”，原本的 “小红” 被替换成了 "小明"。

## 深入分析

搜索和替换文本的概念在计算器的历史中不新鲜。然而, Bash 脚本语言不同寻常的是它可以方便地通过 `sed` 命令来实现。

虽然 `sed` 是非常常见和实用的，但还有其他的替代比如 `awk` ，或者 `perl` 的内置功能。每一个都有自己的优点和适用的场景。

对于 `sed` 的实现被划分为两个部分：模式空间和保持空间。"s/pattern/replacement/" 命令搜索模式空间的内容，并替换匹配的文本。

## 参考信息

更多关于 'sed', 'awk', 以及 'perl' :

- GNU Sed 文档: https://www.gnu.org/software/sed/manual/sed.html
- GNU Awk 用户手册: https://www.gnu.org/software/gawk/manual/gawk.html
- Perl 文档: https://perldoc.perl.org/
  
如果你还想探索更深入的细节, "Mastering Regular Expressions" 是一本很好的参考书目: https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/