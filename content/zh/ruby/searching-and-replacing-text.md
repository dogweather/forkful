---
title:                "搜索和替换文本"
html_title:           "Kotlin: 搜索和替换文本"
simple_title:         "搜索和替换文本"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

---

## 什么和为什么？
搜索和替换文本是在字符串或文件中找到指定内容并更改的程序设计操作。程序员使用它来提高代码的效率和自动化。

## 如何操作：
在 Ruby 中，我们使用 `gsub` 方法搜索和替换文本：

```Ruby
str = 'Hello, World!'
new_str = str.gsub('World', 'Ruby')
puts new_str
```
输出结果:

```Ruby
'Hello, Ruby!'
```

这段代码中，我们使用 `gsub` 方法将 "World" 替换成了 "Ruby"。

## 深入探讨
搜索和替换文本在计算机科学历史中有着悠久的历史。由 ed 编辑器首次引入，现在已经变成几乎所有编程语言的核心特性。

Ruby 提供多种途径执行这项操作。“gsub” 是最常用的方法，但你也可以尝试使用 "replace" 或 "tr" 方法，依照实际需求。

“gsub” 方法的实现细节来源于 Regexp 类，该类是 Ruby 标准库的一部分。这是一个复杂的类，但它提供了强大的正则表达式功能，这使得搜索和替换在 Ruby中非常易用。

## 扩展阅读：
1. Ruby 文档 - `gsub` 方法: [https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
2. Regexp 类: [https://ruby-doc.org/core-2.7.0/Regexp.html](https://ruby-doc.org/core-2.7.0/Regexp.html)

---