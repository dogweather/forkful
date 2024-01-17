---
title:                "提取子字符串"
html_title:           "Ruby: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

提取子串是指从一个字符串中获取指定的一部分字符。程序员经常这样做是因为当他们需要处理大量的文本时，只有部分字符是需要的，每次处理整个字符串会浪费时间和资源。

## 如何实现？

在Ruby中，我们可以使用```[]```方法来提取子串。这个方法接受两个参数，第一个是起始位置，第二个是子串的长度。例如，我们有一个字符串"Hello World"，想要提取"Hello"作为子串，我们可以这样做：
```
string = "Hello World"
substring = string[0, 5]
puts substring
```
输出将会是："Hello"

## 深入探究

提取子串的想法可以追溯到低级语言，比如C。在C中，提取子串的方法有点复杂，需要使用指针来实现。在高级语言中，比如Ruby，我们有更简单的方法来提取子串。

除了使用```[]```方法，还有其他方法可以提取子串，比如使用正则表达式。这种方法可以更灵活，但是需要一定的正则表达式知识。

## 参考资料

[Ruby String Documentation](https://ruby-doc.org/core-2.7.0/String.html)

[Ruby Regex Documentation](https://ruby-doc.org/core-2.7.0/Regexp.html)