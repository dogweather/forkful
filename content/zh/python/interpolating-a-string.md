---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？ (What & Why?)

字符串插值是一种编程模式，它通过将值插入到已有的字符串中来创建新的字符串。编程人员之所以使用它，是因为能够帮助他们有效地动态捕捉和展示信息。

## 如何操作: (How to)

让我们通过Python代码来看看如何进行字符串插值。Python提供了不同的方式来进行字符串插值，包括使用 f-strings（一种在Python 3.6中引入的新的字符串格式化方式）和 format() 函数。

使用 f-strings:

```Python
name = "John"
age = 30
print(f"Hello, my name is {name} and I'm {age} years old.")
```

输出如下:

```
Hello, my name is John and I'm 30 years old.
```

使用 format() 函数:

```Python
name = "John"
age = 30
print("Hello, my name is {} and I'm {} years old".format(name, age))
```
输出如下:
```
Hello, my name is John and I'm 30 years old.
```

## 深入研究 (Deep Dive)

字符串插值是自计算机编程早期以来就存在的一种技术，并在许多编程语言中都得到了实现，如C、Perl、Ruby和现代的Python。除了使用f-strings 和 format() 函数，你也可以使用旧的％-formatting方式进行字符串插值。

在％-formatting中，你需要使用 %操作符并提供一个元组作为值：

```Python
name = "John"
age = 30
print("Hello, my name is %s and I'm %d years old." % (name, age))
```

在Python 3.8及以后的版本中，它已经被看作是一种老旧的风格，并且在新的代码中尽可能少的使用这种方法。

## 延伸阅读 (See Also)

如果你想获得更多关于字符串插值的信息，可以访问以下链接：

- [Python官方文档](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)
- [Real Python 的文章](https://realpython.com/python-f-strings/)
- [Python Tips 的文章](http://book.pythontips.com/en/latest/f_string.html)