---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么是字符串长度以及为什么要找到它呢？

字符串长度基本上就是字符串中的字符数量。当我们需要比较、排序或者操作字符串时，知道字符串长度是极为重要的。

## 如何操作：

```Ruby
# 我们有一个字符串
str = "Hello, World!"

# 我们可以通过调用 length 方法来找出字符串的长度
str_length = str.length

puts str_length  # 输出： 13
```

简单直接，你可以看到输出的结果正是 "Hello, World!" 这个字符串中字符的数量。

## 深入解析：

找出字符串长度的需求可以追溯到古老的编程语言。不同的编程语言可能有不同定义和实现测量字符串长度的方法，比如C++和Java中使用的是 `strlen` 和 `length` 方法。

在Ruby中，我们还有其他几种方式可以得到同样的结果，比如 `size` 方法：

```Ruby
str = "Hello, World!"
str_size = str.size

puts str_size  # 输出： 13
```

关于字符串长度的实现细节，Ruby内部对字符串实现的方式决定了 `length` 和 `size` 的效率，它们实质上只是读取了字符串对象的一个内部计数器。

## 查看更多：

详细文档，请查看[Ruby 官方文档](https://ruby-doc.org/core-2.7.0/String.html)

Ruby教程，查阅[菜鸟教程](https://www.runoob.com/ruby/ruby-string.html)

对于字符串长度在实战中的应用，你可以访问[StackOverflow](https://stackoverflow.com/questions/2336810/ruby-size-vs-length)