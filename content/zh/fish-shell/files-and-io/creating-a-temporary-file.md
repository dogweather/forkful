---
date: 2024-01-20 17:40:01.433272-07:00
description: "How to (\u5982\u4F55\u64CD\u4F5C) Fish Shell \u7684\u4E34\u65F6\u6587\
  \u4EF6\u5B9E\u8DF5\u6CBF\u88AD\u4E86Unix\u4F20\u7EDF\uFF0C`mktemp` \u547D\u4EE4\u6765\
  \u81EA\u65E9\u671F\u7684Unix\u7CFB\u7EDF\uFF0C\u5E76\u88AB\u5927\u591A\u6570\u73B0\
  \u4EE3Unix-like\u7CFB\u7EDF\u91C7\u7528\u3002\u4F5C\u4E3A\u5176\u4ED6\u9009\u9879\
  \uFF0C`tempfile` \u6216\u76F4\u63A5\u5728 `/tmp` \u76EE\u5F55\u4E0B\u521B\u5EFA\u72EC\
  \u4E00\u65E0\u4E8C\u7684\u6587\u4EF6\u540D\u4E5F\u662F\u53EF\u80FD\u7684\u3002\u5177\
  \u4F53\u5230\u5B9E\u73B0\uFF0C`mktemp`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.427029-06:00'
model: gpt-4-1106-preview
summary: "How to (\u5982\u4F55\u64CD\u4F5C) Fish Shell \u7684\u4E34\u65F6\u6587\u4EF6\
  \u5B9E\u8DF5\u6CBF\u88AD\u4E86Unix\u4F20\u7EDF\uFF0C`mktemp` \u547D\u4EE4\u6765\u81EA\
  \u65E9\u671F\u7684Unix\u7CFB\u7EDF\uFF0C\u5E76\u88AB\u5927\u591A\u6570\u73B0\u4EE3\
  Unix-like\u7CFB\u7EDF\u91C7\u7528\u3002\u4F5C\u4E3A\u5176\u4ED6\u9009\u9879\uFF0C\
  `tempfile` \u6216\u76F4\u63A5\u5728 `/tmp` \u76EE\u5F55\u4E0B\u521B\u5EFA\u72EC\u4E00\
  \u65E0\u4E8C\u7684\u6587\u4EF6\u540D\u4E5F\u662F\u53EF\u80FD\u7684\u3002\u5177\u4F53\
  \u5230\u5B9E\u73B0\uFF0C`mktemp` \u53EF\u4EE5\u786E\u4FDD\u751F\u6210\u7684\u6587\
  \u4EF6\u540D\u662F\u72EC\u7279\u7684\uFF0C\u4ECE\u800C\u907F\u514D\u6F5C\u5728\u7684\
  \u6587\u4EF6\u540D\u51B2\u7A81\u3002"
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
weight: 21
---

## How to (如何操作)
```Fish Shell
# 创建临时文件
set tmpfile (mktemp)
echo "这是一个临时文件" > $tmpfile
cat $tmpfile

# 输出例子
这是一个临时文件

# 删除临时文件
rm $tmpfile
```

## Deep Dive (深入了解)
Fish Shell 的临时文件实践沿袭了Unix传统，`mktemp` 命令来自早期的Unix系统，并被大多数现代Unix-like系统采用。作为其他选项，`tempfile` 或直接在 `/tmp` 目录下创建独一无二的文件名也是可能的。具体到实现，`mktemp` 可以确保生成的文件名是独特的，从而避免潜在的文件名冲突。

## See Also (参见资源)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils: mktemp](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- [`mktemp` Man Page](https://linux.die.net/man/1/mktemp)
