---
title:                "寻找字符串的长度"
html_title:           "Ruby: 寻找字符串的长度"
simple_title:         "寻找字符串的长度"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么是字符串长度？

字符串长度是指字符串中字符的数量。在编程中，字符串长度通常被用来检测字符串是否符合要求，或者作为一种测量标准。程序员可以通过计算字符串长度来判断它是否符合给定的长度要求，从而有效地处理字符串。

## 如何计算字符串长度：

ruby的String类提供了一个length方法，可以帮助你轻松地计算字符串的长度。下面是一个示例代码和输出：

```ruby
my_string = "Hello World!"
puts my_string.length
```

输出结果将会是：

```ruby
12
```

## 更深入的了解：

计算字符串长度的方法最初是由计算机科学家Edsger Dijkstra在1967年提出的。除了使用length方法外，程序员也可以使用数组的索引来计算字符串长度，或者使用C语言中的strlen函数来计算。

## 相关链接：

更多关于字符串长度的信息，请访问下面的链接：
- [Ruby String类文档](https://ruby-doc.org/core-2.6.3/String.html#method-i-length)
- [关于计算字符串长度的历史背景](https://en.wikipedia.org/wiki/String_length#History)
- [使用数组索引计算字符串长度](https://www.educative.io/edpresso/how-to-find-the-length-of-a-string-in-ruby)
- [C语言中的strlen函数介绍](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)