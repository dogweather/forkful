---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:10.692606-07:00
description: "\u5728 Python \u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\
  \u662F\u6307\u5728\u6267\u884C\u8BFB\u53D6\u6216\u5199\u5165\u6587\u4EF6\u7B49\u64CD\
  \u4F5C\u4E4B\u524D\uFF0C\u9A8C\u8BC1\u6587\u4EF6\u7CFB\u7EDF\u4E2D\u6587\u4EF6\u5939\
  \u7684\u5B58\u5728\u6027\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\
  \u907F\u514D\u8BF8\u5982`FileNotFoundError`\u8FD9\u6837\u7684\u9519\u8BEF\uFF0C\u786E\
  \u4FDD\u5E94\u7528\u7A0B\u5E8F\u5728\u5C1D\u8BD5\u4E0E\u76EE\u5F55\u4EA4\u4E92\u65F6\
  \u80FD\u53EF\u9760\u5730\u8FD0\u884C\uFF0C\u800C\u4E0D\u4F1A\u5D29\u6E83\u3002"
lastmod: '2024-03-13T22:44:47.271948-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Python \u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u662F\
  \u6307\u5728\u6267\u884C\u8BFB\u53D6\u6216\u5199\u5165\u6587\u4EF6\u7B49\u64CD\u4F5C\
  \u4E4B\u524D\uFF0C\u9A8C\u8BC1\u6587\u4EF6\u7CFB\u7EDF\u4E2D\u6587\u4EF6\u5939\u7684\
  \u5B58\u5728\u6027\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u907F\
  \u514D\u8BF8\u5982`FileNotFoundError`\u8FD9\u6837\u7684\u9519\u8BEF\uFF0C\u786E\u4FDD\
  \u5E94\u7528\u7A0B\u5E8F\u5728\u5C1D\u8BD5\u4E0E\u76EE\u5F55\u4EA4\u4E92\u65F6\u80FD\
  \u53EF\u9760\u5730\u8FD0\u884C\uFF0C\u800C\u4E0D\u4F1A\u5D29\u6E83\u3002."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

## 什么和为什么？
在 Python 中检查目录是否存在是指在执行读取或写入文件等操作之前，验证文件系统中文件夹的存在性。程序员这样做是为了避免诸如`FileNotFoundError`这样的错误，确保应用程序在尝试与目录交互时能可靠地运行，而不会崩溃。

## 如何操作：
Python 提供了使用 `os` 和 `pathlib` 模块来检查目录是否存在的原生方法。以下是两种方法的示例：

### 使用 `os` 模块
```python
import os

# 指定目录路径
dir_path = "/path/to/directory"

# 检查目录是否存在
if os.path.isdir(dir_path):
    print(f"目录 {dir_path} 存在。")
else:
    print(f"目录 {dir_path} 不存在。")
```

### 使用 `pathlib` 模块
```python
from pathlib import Path

# 指定目录路径
dir_path = Path("/path/to/directory")

# 检查目录是否存在
if dir_path.is_dir():
    print(f"目录 {dir_path} 存在。")
else:
    print(f"目录 {dir_path} 不存在。")
```

### 第三方库
尽管 Python 的标准库足以检查目录是否存在，像 `pathlib2` 这样的库可以成为跨 Python 版本一致性或附加功能方面的替代方法。

***注意：*** 截至最新的 Python 版本，对于大多数用例而言，`pathlib` 已经足够健壮，使得第三方库对于这项特定任务变得不那么必要。
