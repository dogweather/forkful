---
aliases:
- /zh/python/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:14.435228-07:00
description: "\u5728 Python \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\u662F\u6307\
  \u5C06\u7A0B\u5E8F\u7684\u9519\u8BEF\u6D88\u606F\u6216\u8BCA\u65AD\u6D88\u606F\u5BFC\
  \u5411\u9519\u8BEF\u6D41\uFF08`stderr`\uFF09\uFF0C\u4E0E\u6807\u51C6\u8F93\u51FA\
  \uFF08`stdout`\uFF09\u5206\u5F00\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u5C06\u6B63\u5E38\u7A0B\u5E8F\u8F93\u51FA\u4E0E\u9519\u8BEF\u6D88\u606F\
  \u5206\u5F00\uFF0C\u4FBF\u4E8E\u8C03\u8BD5\u548C\u65E5\u5FD7\u5206\u6790\u3002"
lastmod: 2024-02-18 23:08:58.805200
model: gpt-4-0125-preview
summary: "\u5728 Python \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\u662F\u6307\u5C06\
  \u7A0B\u5E8F\u7684\u9519\u8BEF\u6D88\u606F\u6216\u8BCA\u65AD\u6D88\u606F\u5BFC\u5411\
  \u9519\u8BEF\u6D41\uFF08`stderr`\uFF09\uFF0C\u4E0E\u6807\u51C6\u8F93\u51FA\uFF08\
  `stdout`\uFF09\u5206\u5F00\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u5C06\u6B63\u5E38\u7A0B\u5E8F\u8F93\u51FA\u4E0E\u9519\u8BEF\u6D88\u606F\u5206\
  \u5F00\uFF0C\u4FBF\u4E8E\u8C03\u8BD5\u548C\u65E5\u5FD7\u5206\u6790\u3002"
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
---

{{< edit_this_page >}}

## 什么和为什么？
在 Python 中写入标准错误是指将程序的错误消息或诊断消息导向错误流（`stderr`），与标准输出（`stdout`）分开。程序员这样做是为了将正常程序输出与错误消息分开，便于调试和日志分析。

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
