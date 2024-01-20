---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 究竟是什么？为什么需要？

"字符串转换成小写"是编程中一个常用操作，它使得程序更容易进行字符串比较。因为在大多数编程语言中，'A'和'a'是完全不同的字符。

## 实际操作：

在 Ruby 中，我们使用 `.downcase` 方法将字符串转为小写。

```Ruby
sentence = "Hello World In Ruby!"
lowercase_sentence = sentence.downcase

puts lowercase_sentence
```

输出：

```Ruby
"hello world in ruby!"
```

非常简单，一行代码就可以实现。


##更深层次了解

在早期的编程语言中，字符串的大小写转换需要写大量代码。Ruby 语言把这个功能封装到了`.downcase` 方法中，使得程序员可以更容易的使用。

相同功能的另一个方法是 `.downcase!` , 它的差别在于会直接修改原字符串：

```Ruby
sentence = "Hello World In Ruby!"
sentence.downcase!

puts sentence
```

输出：

```Ruby
"hello world in ruby!"
```

你可能想，为什么不始终使用 `.downcase!`，因为在某些情况下，我们希望保留原始字符串。

'.downcase' 的实现细节取决于你的 Ruby 版本和对特殊字符的处理。例如，某些版本可能不处理非 ASCII 字符，而其他版本可能会。

## 进一步了解

想了解有关 Ruby 字符串和其他相关方法的更多信息，请参阅 Ruby 文档：

- [Ruby 文档 - String](https://ruby-doc.org/core-2.7.0/String.html)
- [Ruby 文档 - downcase](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase)
- [Ruby 文档 - downcase!](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase-21)