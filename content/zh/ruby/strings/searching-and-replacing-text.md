---
date: 2024-01-20 17:58:41.822330-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1F) Ruby \u63D0\u4F9B\u4E86\u7B80\u5355\
  \u7684\u65B9\u6CD5\u6765\u641C\u7D22\u548C\u66FF\u6362\u5B57\u7B26\u4E32\u3002\u6BD4\
  \u5982 `gsub` \u65B9\u6CD5\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.631979-06:00'
model: gpt-4-1106-preview
summary: "(\u600E\u4E48\u505A\uFF1F) Ruby \u63D0\u4F9B\u4E86\u7B80\u5355\u7684\u65B9\
  \u6CD5\u6765\u641C\u7D22\u548C\u66FF\u6362\u5B57\u7B26\u4E32\u3002\u6BD4\u5982 `gsub`\
  \ \u65B9\u6CD5\uFF1A."
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

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
