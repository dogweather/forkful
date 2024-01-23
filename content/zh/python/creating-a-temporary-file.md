---
title:                "创建临时文件"
date:                  2024-01-20T17:41:18.353113-07:00
model:                 gpt-4-1106-preview
simple_title:         "创建临时文件"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
创建临时文件可以让程序处理临时数据而不必在磁盘上留下痕迹。程序员这样做通常是为了安全性、避免冲突或减少对存储资源的需求。

## How to: (怎么做：)
```Python
import tempfile

# 创建临时文件并写入数据
with tempfile.TemporaryFile(mode='w+t') as t_file:
    t_file.write('Hello Mandarin Readers!')
    t_file.seek(0)  # 回到文件开头
    print(t_file.read())  # 读取并输出内容

# 文件关闭后自动删除
```
输出:
```
Hello Mandarin Readers!
```

## Deep Dive (深入探究)
临时文件历史悠久，Unix 和其他操作系统很早以前就通过 `/tmp` 目录来支持它们。`tempfile` 模块是 Python 标准库的一部分，提供了临时文件和目录的创建。你可以使用 `TemporaryFile`, `NamedTemporaryFile`, `TemporaryDirectory` 等函数。与直接在文件系统上操作相比，这个模块带来更安全、更隔离的环境。临时文件的好处在于，它们通常存放在内存中，并在使用后立即删除，这样可以减少对磁盘的损耗，提高效率。

如果你不使用 `with` 上下文管理器，记得手动关闭文件，否则它们不会被清理。此外，用 `NamedTemporaryFile` 创建的文件有一个可见的文件名，可以在文件系统中找到，直到被关闭。

另一个选择是使用内存中的 `io.StringIO` 或 `io.BytesIO` 对象，但它们不会在文件系统中创建实际文件。

## See Also (另请参阅)
- Python 官方文档中的 `tempfile` 模块介绍：https://docs.python.org/3/library/tempfile.html
- Stack Overflow 上关于在 Python 中创建临时文件的讨论：https://stackoverflow.com/questions/tagged/python+tempfile
