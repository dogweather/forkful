---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Ruby\u63D0\u4F9B\u4E86[\u76F4\u63A5\u7684\
  \u5B57\u7B26\u4E32\u64CD\u4F5C\u65B9\u6CD5](https://docs.ruby-lang.org/en/3.3/String.html)\uFF0C\
  \u5305\u62EC\u5927\u5199\u5316\uFF1A."
lastmod: '2024-04-05T22:38:47.490228-06:00'
model: gpt-4-0125-preview
summary: "//docs.ruby-lang.org/en/3.3/String.html)\uFF0C\u5305\u62EC\u5927\u5199\u5316\
  \uFF1A."
title: "\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199"
weight: 2
---

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
