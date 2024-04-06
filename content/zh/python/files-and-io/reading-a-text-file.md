---
date: 2024-01-20 17:55:00.472023-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u65E9\u671F\uFF0C\u6587\u672C\
  \u6587\u4EF6\u8BFB\u53D6\u4E0E\u64CD\u4F5C\u5BF9\u4E8E\u7F16\u7A0B\u800C\u8A00\u662F\
  \u57FA\u7840\u4EFB\u52A1\u3002\u65E9\u572820\u4E16\u7EAA50\u5E74\u4EE3\uFF0C\u4F7F\
  \u7528\u6253\u5B54\u5361\u7247\u8BFB\u53D6\u6570\u636E\u5DF2\u662F\u5E38\u89C4\u3002\
  Python \u81EA\u8BDE\u751F\u4EE5\u6765\uFF0C\u4E00\u76F4\u63D0\u4F9B\u6613\u7528\u7684\
  \u6587\u4EF6\u8BFB\u5199\u65B9\u6CD5\u3002\u9664\u4E86\u57FA\u672C\u7684\u6587\u4EF6\
  \u8BFB\u53D6\uFF0CPython \u8FD8\u652F\u6301\u5176\u4ED6\u673A\u5236\uFF0C\u4F8B\u5982\
  \ `mmap` \u6216 `io.StringIO`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.627270-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u65E9\u671F\uFF0C\u6587\u672C\u6587\u4EF6\
  \u8BFB\u53D6\u4E0E\u64CD\u4F5C\u5BF9\u4E8E\u7F16\u7A0B\u800C\u8A00\u662F\u57FA\u7840\
  \u4EFB\u52A1\u3002\u65E9\u572820\u4E16\u7EAA50\u5E74\u4EE3\uFF0C\u4F7F\u7528\u6253\
  \u5B54\u5361\u7247\u8BFB\u53D6\u6570\u636E\u5DF2\u662F\u5E38\u89C4\u3002Python \u81EA\
  \u8BDE\u751F\u4EE5\u6765\uFF0C\u4E00\u76F4\u63D0\u4F9B\u6613\u7528\u7684\u6587\u4EF6\
  \u8BFB\u5199\u65B9\u6CD5\u3002\u9664\u4E86\u57FA\u672C\u7684\u6587\u4EF6\u8BFB\u53D6\
  \uFF0CPython \u8FD8\u652F\u6301\u5176\u4ED6\u673A\u5236\uFF0C\u4F8B\u5982 `mmap`\
  \ \u6216 `io.StringIO` \u5BF9\u4E8E\u7279\u5B9A\u60C5\u51B5\u66F4\u9AD8\u6548\u3002\
  \u81F3\u4E8E\u5B9E\u73B0\u7EC6\u8282\uFF0CPython \u4F1A\u5728\u5E55\u540E\u5904\u7406\
  \u8BB8\u591A\u590D\u6742\u6027\uFF0C\u6BD4\u5982\u6587\u4EF6\u7F16\u7801\u6216\u7CFB\
  \u7EDF\u5DEE\u5F02\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

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
