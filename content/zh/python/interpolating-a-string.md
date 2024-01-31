---
title:                "字符串插值"
date:                  2024-01-28T21:23:54.472553-07:00
model:                 gpt-4-0125-preview
simple_title:         "字符串插值"

category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/interpolating-a-string.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
字符串内插是一种在字符串常量中嵌入表达式的方法。程序员使用它来动态地将值插入到字符串中，这使得代码比传统的字符串连接更加易读和清洁。

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
