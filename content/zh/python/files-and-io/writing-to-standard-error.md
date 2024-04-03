---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:14.435228-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A #."
lastmod: '2024-03-13T22:44:47.274232-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

## 如何操作：


### 使用 `sys.stderr`
Python 的内置 `sys` 模块允许显式写入`stderr`。这种方法适用于简单的错误消息或诊断信息。

```python
import sys

sys.stderr.write('Error: Something went wrong.\n')
```
示例输出（到 stderr）：
```
Error: Something went wrong.
```

### 使用 `print` 函数
Python 的 `print` 函数可以通过指定 `file` 参数将其输出重定向到 `stderr`。这种方法利用了 `print` 的用户友好特性，同时处理错误消息。
```python
from sys import stderr

print('Error: Failure in module.', file=stderr)
```
示例输出（到 stderr）：
```
Error: Failure in module.
```

### 使用 `logging` 模块
对于更全面的解决方案，Python 的 `logging` 模块可以将消息导向 `stderr` 以及更多，如写入文件或自定义消息格式。这种方法最适合需要不同日志级别、消息格式或目标的应用。
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Error: Database connection failed.')
```
示例输出（到 stderr）：
```
ERROR:__main__:Error: Database connection failed.
```

### 第三方库：`loguru`
`loguru` 是一个受欢迎的第三方库，它简化了 Python 应用中的日志记录。它自动将错误定向到 `stderr`，除了其他功能。

要使用 `loguru`，首先通过 pip 安装它：
```shell
pip install loguru
```

然后，按如下方式将其纳入你的 Python 脚本：
```python
from loguru import logger

logger.error('Error: Failed to open file.')
```
示例输出（到 stderr）：
```
2023-04-05 12:00:00.000 | ERROR    | __main__:<module>:6 - Error: Failed to open file.
```
