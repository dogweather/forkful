---
title:                "将字符串转换为小写"
html_title:           "Gleam: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 这是什么？为什么要做这个？

将字符串转换为小写形式是一种经常被程序员用到的方法。它可以将字符串中的所有大写字母转换为小写字母，从而使得字符串的格式统一并且易于处理。这在比较字符串或者进行搜索操作时特别有用。

# 如何做？

使用Gleam编程语言可以轻松地实现将字符串转换为小写形式。下面是一个简单的示例代码：

```Gleam
str = "HeLLo WoRLd"
str_lower = str.to_lower()

// Output: "hello world"
```

## 深入探讨

字符串的大小写转换一直是程序员们经常面对的问题。在过去，一些编程语言需要使用循环来逐个字符检查并转换大小写，这就导致了一些性能上的损失。而现在，Gleam提供了to_lower函数，可以帮助程序员们更加高效地完成这一任务。

另外，还有一些其他的方法可以完成字符串的大小写转换，如使用正则表达式或者直接使用内置函数。但是，在Gleam中使用to_lower函数是比较实用和高效的选择。

## 参考链接

- [Gleam官方文档](https://gleam.run/)
- [Gleam版本控制仓库](https://github.com/gleam-lang/gleam)