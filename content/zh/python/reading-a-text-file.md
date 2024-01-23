---
title:                "阅读文本文件"
date:                  2024-01-20T17:55:00.472023-07:00
model:                 gpt-4-1106-preview
simple_title:         "阅读文本文件"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/reading-a-text-file.md"
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
