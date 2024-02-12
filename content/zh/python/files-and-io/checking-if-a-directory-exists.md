---
title:                "检查目录是否存在"
aliases: - /zh/python/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:10.692606-07:00
model:                 gpt-4-0125-preview
simple_title:         "检查目录是否存在"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
