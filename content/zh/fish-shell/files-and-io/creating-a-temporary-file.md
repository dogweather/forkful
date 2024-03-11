---
date: 2024-01-20 17:40:01.433272-07:00
description: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u5C31\u662F\u751F\u6210\u4E00\u4E2A\
  \u77ED\u6682\u5B58\u5728\uFF0C\u5904\u7406\u4E2D\u95F4\u6570\u636E\u7684\u6587\u4EF6\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u907F\u514D\u5E72\u6270\
  \u6B63\u5F0F\u8FD0\u4F5C\u4E2D\u7684\u6570\u636E\uFF0C\u6216\u8005\u6D4B\u8BD5\u4EE3\
  \u7801\u65F6\u4E0D\u7834\u574F\u6301\u4E45\u6570\u636E\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:22.096018-06:00'
model: gpt-4-1106-preview
summary: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u5C31\u662F\u751F\u6210\u4E00\u4E2A\
  \u77ED\u6682\u5B58\u5728\uFF0C\u5904\u7406\u4E2D\u95F4\u6570\u636E\u7684\u6587\u4EF6\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u907F\u514D\u5E72\u6270\
  \u6B63\u5F0F\u8FD0\u4F5C\u4E2D\u7684\u6570\u636E\uFF0C\u6216\u8005\u6D4B\u8BD5\u4EE3\
  \u7801\u65F6\u4E0D\u7834\u574F\u6301\u4E45\u6570\u636E\u3002"
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
---

{{< edit_this_page >}}

## What & Why? (为什么以及为什么？)
创建临时文件就是生成一个短暂存在，处理中间数据的文件。程序员这么做是为了避免干扰正式运作中的数据，或者测试代码时不破坏持久数据。

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
