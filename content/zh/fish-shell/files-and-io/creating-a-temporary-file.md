---
title:                "创建临时文件"
aliases:
- /zh/fish-shell/creating-a-temporary-file.md
date:                  2024-01-20T17:40:01.433272-07:00
model:                 gpt-4-1106-preview
simple_title:         "创建临时文件"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/creating-a-temporary-file.md"
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
