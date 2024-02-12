---
title:                "获取字符串的长度"
aliases:
- /zh/python/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:09.756981-07:00
model:                 gpt-4-1106-preview
simple_title:         "获取字符串的长度"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么与为什么？
在Python中，获取字符串长度的操作就是确定字符串中字符的数目。为了判断用户输入的合法性、裁剪或格式化输出等，程序员经常需要知道字符串的具体长度。

## 如何操作：
```python
# 示例代码：

# 定义一个字符串
string = "你好世界"

# 使用len()函数获取字符串长度
length = len(string)

# 打印结果
print(length)  # 输出：4
```
注意：在Python中，`len()`函数直接返回字符串中的字符数量。

## 深入探讨
**历史背景**：自Python诞生以来，`len()`函数一直是检测序列长度的标准做法，其中包括字符串、列表、元组等。在Python的设计哲学中，简洁通常是首选。

**替代方案**：虽然`len()`是最常用的方法，但你也可以通过遍历字符串并计数来找到长度。这种方式效率更低，不是首选。

```python
# 字符串长度的替代计算方法：
string = "你好世界"
length = 0
for character in string:
    length += 1
print(length)  # 输出：4
```

**实现细节**：`len()`函数在CPython的底层实现中，对于字符串对象有一个`ob_size`的属性，该属性已经存储了字符串的长度，因此调用`len()`可以快速返回结果，不需要遍历整个字符串。

## 相关资源
- Python 官方文档 [`len()` 函数](https://docs.python.org/3/library/functions.html#len)
- W3Schools Python 字符串教程：[Python Strings](https://www.w3schools.com/python/python_strings.asp)
- GeeksforGeeks 关于 Python 字符串操作的更多信息：[Python String](https://www.geeksforgeeks.org/python-strings/)
