---
title:                "使用正则表达式"
aliases:
- /zh/python/using-regular-expressions/
date:                  2024-02-03T19:17:52.779578-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用正则表达式"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
正则表达式（regex）是用于匹配字符串中字符组合的模式。程序员利用它们基于定义的模式搜索、编辑或操作文本，对于数据验证、解析或转换等任务来说，它们是不可或缺的。

## 如何使用：
在Python中使用正则表达式涉及到`re`模块，该模块提供了一系列函数，用于使用正则表达式处理文本。

### 基本模式匹配
要在字符串中搜索模式，请使用`re.search()`。当找到模式时，它返回一个匹配对象，否则返回`None`。
```python
import re

text = "Learn Python programming"
match = re.search("Python", text)
if match:
    print("模式找到了！")
else:
    print("模式未找到。")
```
输出：
```
模式找到了！
```

### 编译正则表达式
对于同一模式的重复使用，首先使用`re.compile()`进行编译，以获得更好的性能。
```python
pattern = re.compile("Python")
match = pattern.search("Learn Python programming")
if match:
    print("编译后的模式找到了！")
```
输出：
```
编译后的模式找到了！
```

### 分割字符串
要在每个正则表达式模式匹配处分割字符串，请使用`re.split()`。
```python
result = re.split("\s", "Python is fun")
print(result)
```
输出：
```
['Python', 'is', 'fun']
```

### 查找所有匹配项
要查找模式的所有非重叠出现，请使用`re.findall()`。
```python
matches = re.findall("n", "Python programming")
print(matches)
```
输出：
```
['n', 'n']
```

### 替换文本
使用`re.sub()`来替换模式的出现位置为一个新字符串。
```python
replaced_text = re.sub("fun", "awesome", "Python is fun")
print(replaced_text)
```
输出：
```
Python is awesome
```

### 第三方库
虽然Python的内置`re`模块很强大，但像`regex`这样的第三方库提供了更多特性和提升的性能。要使用`regex`，通过pip安装它（`pip install regex`）并在代码中导入它。

```python
import regex

text = "Learning Python 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"找到版本：{match.group(1)}")
```
输出：
```
找到版本：3.8
```
