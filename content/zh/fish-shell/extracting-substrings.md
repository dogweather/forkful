---
title:                "提取子字符串"
html_title:           "Fish Shell: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
提取子字符串是什么？它指的是从一个字符串中获取部分内容。程序员之所以这样做，是因为它能够帮助他们从复杂、长的字符串中获取所需的特定信息。

## 如何：
提取子字符串在Fish Shell中是一个简单的任务，只需使用内置的子字符串命令即可。以下是一个例子，假设我们有一个字符串 "Hello Mandarin readers!"，我们想要提取 "Mandarin":
```
set string "Hello Mandarin readers!"
echo $string[6..13]
```

输出将会是：
```
Mandarin
```

## 深入了解：
提取子字符串作为一种技术已经存在了很长一段时间，它可以追溯到早期的Unix系统。除了使用Fish Shell内置命令之外，也可以使用其他编程语言来实现提取子字符串的功能，如Python、Java等。在实际应用中，提取子字符串常用于从大型数据文本中提取特定的信息。

## 参考链接：
- [Fish Shell Github Repository](https://github.com/fish-shell/fish-shell)
- [Python字符串提取](https://www.geeksforgeeks.org/python-string-extraction/)