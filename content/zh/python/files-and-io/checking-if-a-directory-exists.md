---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:10.692606-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Python \u63D0\u4F9B\u4E86\u4F7F\u7528\
  \ `os` \u548C `pathlib` \u6A21\u5757\u6765\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\
  \u5728\u7684\u539F\u751F\u65B9\u6CD5\u3002\u4EE5\u4E0B\u662F\u4E24\u79CD\u65B9\u6CD5\
  \u7684\u793A\u4F8B\uFF1A."
lastmod: '2024-03-13T22:44:47.271948-06:00'
model: gpt-4-0125-preview
summary: "Python \u63D0\u4F9B\u4E86\u4F7F\u7528 `os` \u548C `pathlib` \u6A21\u5757\
  \u6765\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u7684\u539F\u751F\u65B9\u6CD5\
  \u3002\u4EE5\u4E0B\u662F\u4E24\u79CD\u65B9\u6CD5\u7684\u793A\u4F8B\uFF1A\n"
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

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
