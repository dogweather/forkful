---
title:                "搜索和替换文本"
date:                  2024-01-20T17:58:41.822330-07:00
model:                 gpt-4-1106-preview
simple_title:         "搜索和替换文本"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
搜索和替换文本是编程中修改字符串内容的操作。程序员这么做多半是为了自动化地修正错误、更新信息或者改变数据格式。

## How to: (怎么做？)
Ruby 提供了简单的方法来搜索和替换字符串。比如 `gsub` 方法：

```Ruby
original_string = "Hello, world! Ruby is fun."
# Return a new string with all occurrences of 'world' replaced by 'Mandarin readers'
new_string = original_string.gsub('world', 'Mandarin readers')
puts new_string
# => "Hello, Mandarin readers! Ruby is fun."
```

你也可以用正则表达式来匹配模式：

```Ruby
# Replace any word that starts with 'f' with 'Ruby'
fancy_string = "Fancy Fridays are fun for friends."
puts fancy_string.gsub(/\bf\w*/, 'Ruby')
# => "Ruby Ruby are Ruby for Ruby."
```

## Deep Dive (深入探究)
在 Ruby 早期版本中，`sub` 和 `gsub` 方法就被引入，它们是搜索和替换的基础工具。`sub` 替换第一个匹配的字符串，而 `gsub` 替换所有匹配的字符串。

其他语言有类似的函数，比如 Python 的 `replace` 和 JavaScript 的 `replace`。

执行替换时，如果只是修改原字符串用 `gsub!` 或 `sub!` 会更快。

Ruby 的正则表达式是一个强大的工具，可以用来定义复杂的搜索模式，比如大小写不敏感或者选择性匹配。

## See Also (另请参阅)
- Ruby 官方文档中关于 [String#sub](https://ruby-doc.org/core-2.7.1/String.html#method-i-sub) 和 [String#gsub](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub) 的页面。
- [Regexp](https://ruby-doc.org/core-2.7.1/Regexp.html) 文档，详细介绍了正则表达式的使用。
- [Rubular](http://rubular.com/) 一个基于 Ruby 正则表达式的测试工具。