---
title:                "使用正则表达式"
html_title:           "Ruby: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

Regular Expressions in Ruby: Simplifying Your Programming Life

## 什么是正则表达式？为什么程序员要用它？
正则表达式是一种强大的字符串匹配工具，它可以帮助程序员快速、准确地在文本中查找、替换、提取符合特定模式的内容。程序员经常使用正则表达式来简化复杂的字符串处理任务，节省大量时间和精力。

## 如何使用正则表达式：
```Ruby
string = "Hello World123"

# 使用正则表达式匹配并替换
string.gsub!(/[^\w\s]/, '') # 删除所有非字母数字和空格的字符
puts string # 输出: HelloWorld123

# 使用正则表达式提取内容
string = "My email is example@example.com"
regex = /(?<=My email is )\S+/
puts string.match(regex) # 输出: example@example.com
```

## 深入了解：
正则表达式最早由计算机科学家肯·汤普森和罗·贝利发明于20世纪60年代，它是一种通用的模式匹配语言，被应用在各种不同的编程语言和软件中。除了Ruby自带的正则表达式引擎外，程序员还可以选择使用其他第三方库，如Nokogiri和Pcre，来完成更复杂的字符串处理任务。

## 相关资料：
- [Grep and Regex – A Quick Tutorial](https://www.digitalocean.com/community/tutorials/using-grep-and-regex-in-the-linux-or-unix-command-line)
- [Ruby Regular Expressions](https://ruby-doc.org/core-3.0.0/Regexp.html)
- [Mastering Regular Expressions (book)](https://regex.info/blog/)