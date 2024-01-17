---
title:                "搜索和替换文本"
html_title:           "Ruby: 搜索和替换文本"
simple_title:         "搜索和替换文本"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

#什么和为什么？
搜索和替换文本是程序员常用的技术，它允许你检索和替换文本中的特定内容。程序员之所以这样做，是因为它可以帮助他们快速有效地修改并更新大量的文本数据。 

#怎么做？
````Ruby
# 在Ruby里，你可以使用 .gsub 方法来做搜索和替换文本。
# 下面是一个例子，我们将把 "Hello" 替换成 "你好"：
puts "Hello everyone!".gsub("Hello", "你好") 
# output => “你好 everyone!"

# 你也可以使用正则表达式来指定要替换的内容。下面的例子将会把所有的数字替换成 x：
puts "12345".gsub(/\d/, "x") 
# output => “xxxxx”
````

#深度挖掘
搜索和替换文本的技术可以追溯到早期的文本编辑器和命令行工具，如sed和awk。在现代编程中，它仍然是一种非常有用的技术，但也有一些替代方法，如使用正则表达式来匹配和替换文本。在Ruby中，提供了多种方法来实现搜索和替换文本，如使用.gsub方法、String类的.gsub方法以及正则表达式。使用这些技术，你可以快速地修改和更新文本数据。

#查看更多
- [Ruby String文档](https://ruby-doc.org/core-2.5.1/String.html#method-i-gsub)
- [正则表达式教程](https://www.w3schools.com/ruby/ruby_regex.asp)
- [sed和awk介绍](https://www.tutorialspoint.com/unix_commands/sed.htm)