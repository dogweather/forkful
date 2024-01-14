---
title:                "Ruby: 将字符串大写化"
simple_title:         "将字符串大写化"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，有时候我们需要将字符串的首字母大写，以便在显示数据时更加美观。Ruby中有一个内置的方法可以帮助我们轻松实现这个功能。

## 如何做

首先，我们需要创建一个字符串变量，例如：`str = "hello world"`。然后，我们可以使用`.capitalize`方法来将字符串的首字母大写，例如：`str.capitalize`。最后，我们可以通过`puts`语句来输出结果：`puts str`。

```Ruby
str = "hello world"
str.capitalize 
puts str
```

输出结果为：Hello world。

## 深入探究

在Ruby中，`.capitalize`方法的工作原理是将字符串的第一个字符转换为大写，而其余的字符则保持不变。如果我们想要将整个字符串的每个单词的首字母大写，可以使用`.split`和`.map`方法一起使用来实现。

```Ruby
str = "hello world"
split_str = str.split(" ")
capital_str = split_str.map(&:capitalize)
puts capital_str.join(" ")
```

输出结果为：Hello World。

## 参考链接

- [Ruby API文档](https://ruby-doc.org/core-2.6/String.html#method-i-capitalize)
- [Ruby String.capitalize方法教程](https://www.tutorialspoint.com/ruby/ruby_string_capitalize.htm)
- [如何在Ruby中将字符串的每个单词首字母大写](https://stackoverflow.com/questions/43411089/how-to-make-only-the-first-letter-of-every-word-in-a-string-upper-case-in-ruby)

## 参见