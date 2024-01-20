---
title:                "将字符串大写"
html_title:           "Python: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 是什么？为什么？

在Python中，大写字符串(translated: Capitalizing a string)可理解为将字符串的首字母转换为大写，其他字母转换为小写。程序员这么做一般出于格式要求或者需要高效进行文本处理。

## 怎么做：

可以使用Python内置的`capitalize()`函数来大写字符串。看下面的代码：

```Python
# 定义一个字符串
s = "hello, world!"

# 使用capitalize()函数
new_s = s.capitalize()

# 打印新的字符串
print(new_s)
```

输出结果是：

```
Hello, world!
```

看到了吧，"hello, world!"变成了"Hello, world!"。只是首字母大写，其他都是小写。

## 深度探究：

A. 历史背景： Python从事实上版本就开始支持字符串的`capitalize()`函数，这表明用Python处理字符串是非常自然和便捷的。

B. 选择： Python还提供了其他一些函数，如`upper()`将所有字母变为大写，或者`title()`将每个单词首字母都变为大写。

```Python
s = "hello, world!"
print(s.upper())  # 输出 "HELLO, WORLD!"
print(s.title())  # 输出 "Hello, World!"
```

C. 实现细节： `capitalize()`函数的实现依赖于Python的Unicode数据，这意味着它运作在多语言环境中都是可行的。但是注意，这并不能完成复杂的文字处理，例如，德语里的"straße"转换后会变成"Straße"，而不是"Straße"。

## 另请参阅：

* [Python 官方文档](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
* [W3Schools Python 字符串 capitalize() 函数](https://www.w3schools.com/python/ref_string_capitalize.asp)