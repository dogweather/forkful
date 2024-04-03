---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "\u5927\u5199\u5316\u4E00\u4E2A\u5B57\u7B26\u4E32\u901A\u5E38\u610F\u5473\
  \u7740\u5C06\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\u8F6C\u6362\u4E3A\
  \u5927\u5199\uFF0C\u5176\u4F59\u7684\u8F6C\u6362\u4E3A\u5C0F\u5199\u3002\u4F46\u6709\
  \u65F6\u5019\uFF0C\u5B83\u4EC5\u4EC5\u610F\u5473\u7740\u786E\u4FDD\u7B2C\u4E00\u4E2A\
  \u5B57\u7B26\u662F\u5927\u5199\u7684\uFF0C\u800C\u4FDD\u6301\u5B57\u7B26\u4E32\u7684\
  \u5176\u4F59\u90E8\u5206\u4E0D\u53D8\u3002\u5766\u7387\u5730\u8BF4\uFF0C\u5728\u6211\
  \u770B\u6765\uFF0C\u8FD9\u662F\u4E00\u4E2A\u6709\u70B9\u6A21\u7CCA\u7684\u672F\u8BED\
  \u3002"
lastmod: '2024-03-25T19:21:58.466442-06:00'
model: gpt-4-0125-preview
summary: "\u5927\u5199\u5316\u4E00\u4E2A\u5B57\u7B26\u4E32\u901A\u5E38\u610F\u5473\
  \u7740\u5C06\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\u8F6C\u6362\u4E3A\
  \u5927\u5199\uFF0C\u5176\u4F59\u7684\u8F6C\u6362\u4E3A\u5C0F\u5199\u3002\u4F46\u6709\
  \u65F6\u5019\uFF0C\u5B83\u4EC5\u4EC5\u610F\u5473\u7740\u786E\u4FDD\u7B2C\u4E00\u4E2A\
  \u5B57\u7B26\u662F\u5927\u5199\u7684\uFF0C\u800C\u4FDD\u6301\u5B57\u7B26\u4E32\u7684\
  \u5176\u4F59\u90E8\u5206\u4E0D\u53D8\u3002\u5766\u7387\u5730\u8BF4\uFF0C\u5728\u6211\
  \u770B\u6765\uFF0C\u8FD9\u662F\u4E00\u4E2A\u6709\u70B9\u6A21\u7CCA\u7684\u672F\u8BED\
  \u3002."
title: "\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199"
weight: 2
---

## 什么及为什么？
大写化一个字符串通常意味着将字符串的第一个字符转换为大写，其余的转换为小写。但有时候，它仅仅意味着确保第一个字符是大写的，而保持字符串的其余部分不变。坦率地说，在我看来，这是一个有点模糊的术语。

## 如何操作：
Ruby提供了[直接的字符串操作方法](https://docs.ruby-lang.org/en/3.3/String.html)，包括大写化：

```ruby
# Ruby 的内置方法
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

非常方便。

Ruby的 `.capitalize` 方法很方便，但只会将第一个字母转换成大写。如果你想要更多控制权，或者想要将字符串中的每个单词都转换为标题式大小写（称为标题大小写），你可能会想要使用Rails ActiveSupport扩展中的 `titleize` 方法，或者自己实现它：

```ruby
# 在 Rails 中使用 ActiveSupport 的 'titleize'
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# 自制的解决方案
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

这个方法将字符串分割成一个单词数组，将每一个都大写，然后用空格将它们连接回去。

就我个人而言，我在我的代码中将这个想法推进得更远。我编写了我自己的[`titleize`方法，它考虑了像“a”和“the”这样的小词](https://github.com/public-law/law_string/blob/master/lib/law_string.rb)。
