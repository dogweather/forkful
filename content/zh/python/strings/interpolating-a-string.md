---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:23:54.472553-07:00
description: "\u5982\u4F55\u505A\uFF1A \u5728Python 3.6\u53CA\u4EE5\u4E0A\u7248\u672C\
  \u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528f-string\u6765\u63D2\u503C\u5B57\u7B26\
  \u4E32\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u505A\u7684\uFF1A."
lastmod: '2024-04-05T22:38:46.421426-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u505A\uFF1A \u5728Python 3.6\u53CA\u4EE5\u4E0A\u7248\u672C\u4E2D\
  \uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528f-string\u6765\u63D2\u503C\u5B57\u7B26\u4E32\
  \u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u505A\u7684\uFF1A."
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

## 如何做：
在Python 3.6及以上版本中，你可以使用f-string来插值字符串。以下是如何做的：

```Python
name = 'Alice'
age = 30
greeting = f"Hello, {name}. You are {age} years old."

print(greeting)
```

输出：
```
Hello, Alice. You are 30 years old.
```

你也可以在花括号内使用表达式：

```Python
a = 5
b = 10
info = f"五加十等于{a + b}，不是{2 * (a + b)}."

print(info)
```

输出：
```
五加十等于15，不是30。
```

## 深入探索
在Python 3.6之前，`.format()`是字符串插值的方式：

```Python
name = 'Bob'
age = 25
greeting = "Hello, {}. You are {} years old.".format(name, age)

print(greeting)
```

老式Python（版本 < 2.6）使用`%`运算符进行插值，这种方式不够直观，并且在处理多个变量时会变得混乱：

```Python
name = 'Carol'
age = 35
greeting = "Hello, %s. You are %d years old." % (name, age)

print(greeting)
```

除了语法更清晰外，f-string还因为它们在运行时被评估然后直接转换成高效的字符串格式操作而更快。`.format()`方法和`%`运算符涉及更多步骤，因此更慢。

## 另请参阅
- [PEP 498 - 字面字符串内插](https://www.python.org/dev/peps/pep-0498/) 了解f-string的官方文档。
- [Python f字符串](https://realpython.com/python-f-strings/) 由Real Python提供的f-string使用教程。
- [Python文档中的.format()方法](https://docs.python.org/3/library/stdtypes.html#str.format) 以了解旧的`.format()`字符串格式化方法。
