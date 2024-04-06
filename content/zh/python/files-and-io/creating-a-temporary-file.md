---
date: 2024-01-20 17:41:18.353113-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1A) \u4E34\u65F6\u6587\u4EF6\u5386\u53F2\
  \u60A0\u4E45\uFF0CUnix \u548C\u5176\u4ED6\u64CD\u4F5C\u7CFB\u7EDF\u5F88\u65E9\u4EE5\
  \u524D\u5C31\u901A\u8FC7 `/tmp` \u76EE\u5F55\u6765\u652F\u6301\u5B83\u4EEC\u3002\
  `tempfile` \u6A21\u5757\u662F Python \u6807\u51C6\u5E93\u7684\u4E00\u90E8\u5206\uFF0C\
  \u63D0\u4F9B\u4E86\u4E34\u65F6\u6587\u4EF6\u548C\u76EE\u5F55\u7684\u521B\u5EFA\u3002\
  \u4F60\u53EF\u4EE5\u4F7F\u7528 `TemporaryFile`, `NamedTemporaryFile`,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.457216-06:00'
model: gpt-4-1106-preview
summary: "(\u600E\u4E48\u505A\uFF1A) \u4E34\u65F6\u6587\u4EF6\u5386\u53F2\u60A0\u4E45\
  \uFF0CUnix \u548C\u5176\u4ED6\u64CD\u4F5C\u7CFB\u7EDF\u5F88\u65E9\u4EE5\u524D\u5C31\
  \u901A\u8FC7 `/tmp` \u76EE\u5F55\u6765\u652F\u6301\u5B83\u4EEC\u3002`tempfile` \u6A21\
  \u5757\u662F Python \u6807\u51C6\u5E93\u7684\u4E00\u90E8\u5206\uFF0C\u63D0\u4F9B\
  \u4E86\u4E34\u65F6\u6587\u4EF6\u548C\u76EE\u5F55\u7684\u521B\u5EFA\u3002\u4F60\u53EF\
  \u4EE5\u4F7F\u7528 `TemporaryFile`, `NamedTemporaryFile`, `TemporaryDirectory` \u7B49\
  \u51FD\u6570\u3002\u4E0E\u76F4\u63A5\u5728\u6587\u4EF6\u7CFB\u7EDF\u4E0A\u64CD\u4F5C\
  \u76F8\u6BD4\uFF0C\u8FD9\u4E2A\u6A21\u5757\u5E26\u6765\u66F4\u5B89\u5168\u3001\u66F4\
  \u9694\u79BB\u7684\u73AF\u5883\u3002\u4E34\u65F6\u6587\u4EF6\u7684\u597D\u5904\u5728\
  \u4E8E\uFF0C\u5B83\u4EEC\u901A\u5E38\u5B58\u653E\u5728\u5185\u5B58\u4E2D\uFF0C\u5E76\
  \u5728\u4F7F\u7528\u540E\u7ACB\u5373\u5220\u9664\uFF0C\u8FD9\u6837\u53EF\u4EE5\u51CF\
  \u5C11\u5BF9\u78C1\u76D8\u7684\u635F\u8017\uFF0C\u63D0\u9AD8\u6548\u7387\u3002"
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
weight: 21
---

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
