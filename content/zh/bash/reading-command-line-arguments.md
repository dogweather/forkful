---
title:                "读取命令行参数"
date:                  2024-01-20T17:55:32.675950-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
在Bash脚本中读取命令行参数是获取用户输入数据的方法之一。程序员这样做是为了让脚本更灵活，可以根据不同输入执行不同操作。

## How to: 如何做？
```Bash
#!/bin/bash
# 保存第一个和第二个命令行参数到变量
first_arg=$1
second_arg=$2

# 打印参数
echo "第一个参数: $first_arg"
echo "第二个参数: $second_arg"
```
保存此脚本为 `example.sh` 并运行：
```Bash
bash example.sh 苹果 香蕉
```
输出：
```
第一个参数: 苹果
第二个参数: 香蕉
```

## Deep Dive: 深入探索
Bash 是在1989年作为GNU计划的一部分诞生的，最初是为了代替Bourne Shell。在处理命令行参数方面，它使用位置参数（$0, $1, ...，其中$0是脚本本身的名称，$1是第一个参数，以此类推）。

除了直接使用位置参数，Bash 还提供了`getopts`和`shift`命令用于更复杂的参数解析。`getopts` 主要用于短选项，而使用 `getopt` 命令可以支持长选项解析。

具体到实现，Bash脚本参数是通过特殊的环境变量（位置参数）暴露给脚本的。`$#`表示参数总数，`$*`和`$@`代表所有参数的列表，但在双引号中展开时有所不同，`$*`会将所有参数视为一个单一字符串而`$@`会保留每个参数作为独立字符串。

## See Also: 参见
- [Bash手册](https://www.gnu.org/software/bash/manual/)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/)
