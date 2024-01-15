---
title:                "将字符串转换为小写"
html_title:           "Ruby: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么
在编程中，我们经常需要将字符串的大小写进行转换，以便于进行比较或者格式化输出。通过把字符串转换为小写，我们可以更容易地对其进行处理和操作。

## 如何操作
使用Ruby编程语言可以很轻松地将字符串转换为小写，下面是一个简单的例子：

```Ruby
my_string = "HELLO WORLD"
puts my_string.downcase #  输出结果为："hello world"
```

在上面的代码中，我们首先定义了一个字符串变量，然后使用下面的 `downcase` 方法将其转换为小写。你也可以使用 `upcase` 方法将字符串转换为大写。

## 深入了解
字符串的大小写转换在 Ruby 中有多种实现方法，比如使用 `capitalize` 方法将首字母转换为大写，使用 `swapcase` 方法交换字符串中的大小写，以及使用 `tr` 方法按照字母表顺序将字符串中的字符进行转换。此外，你还可以通过正则表达式来实现字符串的大小写转换，从而更加灵活地处理字符串。

## 参考链接
- [Ruby 官方文档](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)
- [Ruby String 类方法指南](https://learnrubythehardway.org/book/ex10.html)
- [Ruby 语言参考教程](https://www.ruby-lang.org/zh_cn/documentation/)