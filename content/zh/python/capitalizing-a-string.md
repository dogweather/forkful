---
title:                "字符串大写化"
html_title:           "Python: 字符串大写化"
simple_title:         "字符串大写化"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Capitalizing a String: Python 中的字符串大小写:

## What & Why?

字符串大小写是指将一个字符串的首字母变为大写，其余字母保持不变。程序员们通常会这样做是为了让字符串更容易阅读和可读性更强。

## How to:

要在Python中将字符串转换为大写，可以使用内置的capitalize()函数。下面是一个简单的例子：

```Python
message = "hello world"
print(message.capitalize())
```

输出结果为：

```
Hello world
```

## Deep Dive:

历史上，字符串大小写的概念来源于打字机时代，为了强调一个句子的重点部分，打字员们会将它们的首字母改为大写。在现代编程中，字符串大小写仍然被用来提高可读性。

除了capitalize()函数外，Python还提供了其他方法来实现字符串大小写的转换，比如使用upper()和lower()函数来分别将字符串转为大写和小写。另外，还可以使用string模块中的capwords()函数来将字符串中的每个单词的首字母都转为大写。

## See Also:

- [Python String capitalize()方法文档](https://www.runoob.com/python/att-string-capitalize.html)
- [Python String upper()方法文档](https://www.runoob.com/python/att-string-upper.html)
- [Python String lower()方法文档](https://www.runoob.com/python/att-string-lower.html)
- [Python String capwords()方法文档](https://www.runoob.com/python/att-string-capwords.html)