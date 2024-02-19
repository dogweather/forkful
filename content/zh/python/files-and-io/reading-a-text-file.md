---
aliases:
- /zh/python/reading-a-text-file/
date: 2024-01-20 17:55:00.472023-07:00
description: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6\u5C31\u662F\u8BA9\u7A0B\u5E8F\u4ECE\
  \u6587\u4EF6\u4E2D\u63D0\u53D6\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u4E3B\u8981\u662F\u4E3A\u4E86\u5229\u7528\u6587\u4EF6\u4E2D\u7684\u6570\u636E\u8FDB\
  \u884C\u5206\u6790\u3001\u8F6C\u6362\u6216\u8F93\u51FA\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:58.806108
model: gpt-4-1106-preview
summary: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6\u5C31\u662F\u8BA9\u7A0B\u5E8F\u4ECE\
  \u6587\u4EF6\u4E2D\u63D0\u53D6\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u4E3B\u8981\u662F\u4E3A\u4E86\u5229\u7528\u6587\u4EF6\u4E2D\u7684\u6570\u636E\u8FDB\
  \u884C\u5206\u6790\u3001\u8F6C\u6362\u6216\u8F93\u51FA\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
阅读文本文件就是让程序从文件中提取数据。程序员这样做主要是为了利用文件中的数据进行分析、转换或输出。

## How to: (如何操作：)
```python
# 读取整个文件
with open('example.txt', 'r', encoding='utf-8') as file:
    content = file.read()
    print(content)

# Output:
# 文件中的文本内容...

# 按行读取
with open('example.txt', 'r', encoding='utf-8') as file:
    for line in file:
        print(line.strip())  # 移除换行符

# Output:
# 文件的
# 每一行
# 内容...
```

## Deep Dive (深入探索)
早期，文本文件读取与操作对于编程而言是基础任务。早在20世纪50年代，使用打孔卡片读取数据已是常规。Python 自诞生以来，一直提供易用的文件读写方法。除了基本的文件读取，Python 还支持其他机制，例如 `mmap` 或 `io.StringIO` 对于特定情况更高效。至于实现细节，Python 会在幕后处理许多复杂性，比如文件编码或系统差异。

## See Also (另请参阅)
- [Python 官方文档：文件输入输出](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Python 文本处理入门](https://realpython.com/python-strings/)
- [深入理解 Python 文件操作](https://www.programiz.com/python-programming/file-operation)
