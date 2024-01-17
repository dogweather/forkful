---
title:                "将字符串转换为小写"
html_title:           "Python: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 什么是字符串转换成小写？
字符串转换成小写是一种将所有字母转换为小写形式的操作。这在编程中非常常见，因为小写的字母在计算机中更易于处理。 

# 为什么要做这个？
许多编程语言提供了内置函数，可以将字符串转换成小写。这样做可以在比较字符串时避免大小写不匹配。此外，让所有的字符串都是小写形式也可以提供一致性，使得代码更容易理解和维护。 

# 如何操作： 
你可以使用字符串的lower()方法来将其转换成小写形式。这个方法将返回一个新的字符串，所有的字母都是小写的。让我们看一个例子： 

```
my_string = "HELLO WORLD"
print(my_string.lower())
>>> hello world
```

你也可以使用capitalize()方法来将字符串的首字母转换成大写，同时将其他字母转换成小写。 

```
my_string = "hElLo WoRlD"
print(my_string.capitalize())
>>> Hello world
```

# 深入探讨： 
虽然在计算机科学历史上，使用小写字母的方式是最普遍的，但仍然有一些编程语言选择不同的方式。举个例子，C语言中的字符串不区分大小写，Python中的字符串则是区分大小写的。 

除了使用内置方法，你也可以通过使用ASCII或Unicode编码来手动转换字符串的大小写。此外，你也可以使用第三方库来实现更丰富的转换功能，例如转换特殊字符或处理多个字符串。 

# 参考链接： 
- https://www.programiz.com/python-programming/methods/string/lower
- https://www.geeksforgeeks.org/capitalize-first-letter-of-each-word-in-a-string-in-python/
- https://unicode.org/ 
- https://docs.python.org/3/library/codecs.html#standard-encodings