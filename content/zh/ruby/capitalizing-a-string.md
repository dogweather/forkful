---
title:                "将字符串大写化"
html_title:           "Ruby: 将字符串大写化"
simple_title:         "将字符串大写化"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 啥 & 为何?

字符串的大写化，是让字符串第一个字母变成大写的操作。程序员做这个主要是为了强制遵循文本显示的规则。

## 怎么做:

在Ruby中，你可以使用`capitalize`方法来实现。下面是一些使用示例和结果：
```Ruby
"hello world".capitalize # => "Hello world"
"HELLO WORLD".capitalize # => "Hello world"
"123abc".capitalize # => "123abc"
```
此方法将字符串的首字母转换为大写，其余的转为小写。

## 深入了解

在历史中，电报以及早期的计算机使用全部大写字母，因此变现的时候往往都会采用全部大写。而现在，大多的语标明将首字母大写作为区标题或者人名。

而在Ruby中，除了`capitalize`方法之外，还有一些其他方法：

- `upcase`：全部字母变为大写。比如`"hello".upcase # => "HELLO"`
- `downcase`：全部字母变为小写。比如`"HELLO".downcase # => "hello"`
- `swapcase`：大写字母变小写，小写字母变大写。比如`"Hello".swapcase # => "hELLO"`

## 参考资料

你可以参阅下列链接以更全面地了解Ruby的字符串处理：

- [Ruby官方字符串文档](https://www.ruby-lang.org/zh_cn/documentation/tutorial/strings/)
- [Ruby字符串方法大全](https://ruby-doc.org/core-2.7.2/String.html)
- [Ruby学习笔记: 字符串处理](https://www.w3cschool.cn/ruby/ruby-string-methods.html)