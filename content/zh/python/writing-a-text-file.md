---
title:                "编写文本文件"
date:                  2024-01-19
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"

category:             "Python"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
写入文本文件是将字符串保存到文件中的过程。程序员这么做来储存数据，记录日志，或进行配置。

## How to: 如何做？
```python
# 写入文本到文件
with open('example.txt', 'w', encoding='utf-8') as file:
    file.write("你好，世界！")

# 输出示例: 检查文件内容
with open('example.txt', 'r', encoding='utf-8') as file:
    print(file.read())
```
```
你好，世界！
```

## Deep Dive 深入探索
文本文件写入在历史上一直是数据持久化的基本手段。除了基本的文本写入，还存在如二进制文件写入等替代方案。在实现细节上，Python内置的`open`函数支持多种模式，如追加模式(`'a'`)，这些都由底层操作系统提供支持。

## See Also 另请参阅
- Python 官方文档中的文件操作指南: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- W3Schools 文件处理教程: https://www.w3schools.com/python/python_file_handling.asp
