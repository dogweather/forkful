---
date: 2024-01-20 17:55:32.675950-07:00
description: "How to: \u5982\u4F55\u505A\uFF1F Bash \u662F\u57281989\u5E74\u4F5C\u4E3A\
  GNU\u8BA1\u5212\u7684\u4E00\u90E8\u5206\u8BDE\u751F\u7684\uFF0C\u6700\u521D\u662F\
  \u4E3A\u4E86\u4EE3\u66FFBourne Shell\u3002\u5728\u5904\u7406\u547D\u4EE4\u884C\u53C2\
  \u6570\u65B9\u9762\uFF0C\u5B83\u4F7F\u7528\u4F4D\u7F6E\u53C2\u6570\uFF08$0, $1,\
  \ ...\uFF0C\u5176\u4E2D$0\u662F\u811A\u672C\u672C\u8EAB\u7684\u540D\u79F0\uFF0C\
  $1\u662F\u7B2C\u4E00\u4E2A\u53C2\u6570\uFF0C\u4EE5\u6B64\u7C7B\u63A8\uFF09\u3002\
  \ \u9664\u4E86\u76F4\u63A5\u4F7F\u7528\u4F4D\u7F6E\u53C2\u6570\uFF0CBash\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.188479-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u505A\uFF1F Bash \u662F\u57281989\u5E74\u4F5C\u4E3AGNU\u8BA1\
  \u5212\u7684\u4E00\u90E8\u5206\u8BDE\u751F\u7684\uFF0C\u6700\u521D\u662F\u4E3A\u4E86\
  \u4EE3\u66FFBourne Shell\u3002\u5728\u5904\u7406\u547D\u4EE4\u884C\u53C2\u6570\u65B9\
  \u9762\uFF0C\u5B83\u4F7F\u7528\u4F4D\u7F6E\u53C2\u6570\uFF08$0, $1, ...\uFF0C\u5176\
  \u4E2D$0\u662F\u811A\u672C\u672C\u8EAB\u7684\u540D\u79F0\uFF0C$1\u662F\u7B2C\u4E00\
  \u4E2A\u53C2\u6570\uFF0C\u4EE5\u6B64\u7C7B\u63A8\uFF09\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
