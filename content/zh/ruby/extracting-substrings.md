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

## 为什么

有时候在编写程序时，我们需要从一个字符串中提取特定的部分，这就是提取子字符串的原因。在Ruby中，提取子字符串非常简单，只需要使用几个内置的方法就可以实现。

## 如何做

首先，我们需要一个包含字符串的变量。例如，`message = "你好，世界"`。

#### 提取从索引2开始的所有字符

我们可以使用`message[2..-1]`来提取从索引2到最后一个字符的所有字符。输出结果为`"，世界"`。

#### 提取特定范围的字符

如果我们想要提取特定范围的字符，我们可以使用`message[start_index, length]`来指定开始的索引和需要提取的字符数量。例如，`message[1,3]`将会提取从索引1开始的3个字符，输出结果为`"好，世"`。

#### 使用正则表达式来提取

我们也可以使用正则表达式来提取特定模式的字符串。例如，`message[/好.*界/]`会提取所有在“好”和“界”之间的字符串，输出结果为`"好，世界"`。

## 深入探讨

在Ruby中，提取字符串的方法有很多种，可以满足不同的需求。除了上面提到的方法，还有很多其他有用的方法，如`scan`、`match`和`split`。这些方法在提取特定模式的字符串时非常有用，可以帮助我们更快地处理数据。

## 参考链接

- [Ruby文档-String类](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby文档-Regular Expression](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Ruby Guides-Extracting Substrings](https://www.rubyguides.com/2018/11/extract-substring-in-ruby/)