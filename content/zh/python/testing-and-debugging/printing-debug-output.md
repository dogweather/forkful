---
title:                "打印调试输出"
aliases:
- zh/python/printing-debug-output.md
date:                  2024-01-20T17:53:17.537030-07:00
model:                 gpt-4-1106-preview
simple_title:         "打印调试输出"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
输出调试信息是在代码中打印变量、状态或错误信息的过程，程序员这么做是为了跟踪程序运行过程中的行为，帮助找出问题所在。

## 怎么做：
```Python
# 打印一个简单的字符串到控制台
print("开始调试程序。")

# 打印变量的值
debug_variable = "调试信息"
print(f"变量内容: {debug_variable}")

# 打印错误消息
try:
    1 / 0
except ZeroDivisionError as e:
    print(f"捕捉到错误：{e}")
```
```
开始调试程序。
变量内容: 调试信息
捕捉到错误：division by zero
```

## 深入探究：
在早期的编程年代，调试程序通常依赖于物理打印机输出代码执行的详细记录。现在，print()函数是Python中最基础而广泛使用的调试方法之一，但有时它不够用。其他的调试方法包括使用日志（logging模块），断点调试（pdb模块），或者集成开发环境（IDE）的调试工具。使用print()时，可以通过在变量旁边添加额外的信息，比如变量名或者出现的位置，来更精确地定位问题。在运行程序后，可能需要移除或注释掉这些调试输出。

## 另请参阅：
- Python 官方文档的 logging 模块：https://docs.python.org/3/library/logging.html
- Python 官方文档的 pdb 模块：https://docs.python.org/3/library/pdb.html
- Visual Studio Code 调试说明：https://code.visualstudio.com/docs/python/python-tutorial#_configure-and-run-the-debugger
