---
date: 2024-01-20 17:39:10.442888-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1A) Python\u4F7F\u7528`.lower()`\u65B9\
  \u6CD5\u5C06\u5B57\u7B26\u4E32\u8F6C\u4E3A\u5C0F\u5199\u3002\u4E0B\u9762\u662F\u51E0\
  \u4E2A\u4F8B\u5B50\u548C\u5B83\u4EEC\u7684\u8F93\u51FA\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.597058-06:00'
model: gpt-4-1106-preview
summary: "(\u600E\u4E48\u505A\uFF1A) Python\u4F7F\u7528`.lower()`\u65B9\u6CD5\u5C06\
  \u5B57\u7B26\u4E32\u8F6C\u4E3A\u5C0F\u5199\u3002\u4E0B\u9762\u662F\u51E0\u4E2A\u4F8B\
  \u5B50\u548C\u5B83\u4EEC\u7684\u8F93\u51FA\uFF1A."
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

## How to: (怎么做：)
Python使用`.lower()`方法将字符串转为小写。下面是几个例子和它们的输出：

```Python
original_string = "Hello, World!"
lowercase_string = original_string.lower()
print(lowercase_string)
```
输出：
```
hello, world!
```

如果你想对一个列表中的每个字符串都进行小写转换，可以这么做：

```Python
strings_list = ["Python", "Is", "FUN!"]
lowercase_list = [s.lower() for s in strings_list]
print(lowercase_list)
```
输出：
```
['python', 'is', 'fun!']
```

## Deep Dive (深入了解)
在早期的计算历史中，大小写敏感是个问题，这会导致不一致的数据比较结果。`.lower()`方法起源于C语言的字符串处理函数。在Python中，这一方法提供了一个简单、快速、跨文化的方式去统一字符串数据。

除`.lower()`方法外，还有其他方法可以转换大小写，比如`.casefold()`。`.casefold()`在处理某些多语言环境中的大小写转换时比`.lower()`更为强大。还有如`.upper()`转成大写，`.title()`转成首字母大写。

当然，转化过程要考虑编码。Python 3.x使用Unicode编码，这使得大小写转换更加准确，尤其是在处理国际字符时。

## See Also (另请参阅)
- Python官方文档关于字符串方法： https://docs.python.org/3/library/stdtypes.html#string-methods
- Unicode案例映射表：https://unicode.org/charts/case/
- Python教程关于字符串：https://docs.python.org/3/tutorial/introduction.html#strings
