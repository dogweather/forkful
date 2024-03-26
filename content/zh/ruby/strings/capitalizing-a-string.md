---
title:                "字符串首字母大写"
date:                  2024-03-25T17:31:51.009014-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
将字符串首字母大写通常意味着将字符串的第一个字符转换为大写，其余转换为小写。但有时候，它仅仅意味着确保第一个字符是大写的，而保持字符串的其他部分不变。老实说，在我看来，这是一个有点模糊的术语。

## 如何操作：
Ruby 提供了[简单的字符串操作方法](https://docs.ruby-lang.org/en/3.3/String.html)，包括首字母大写：

```ruby
# Ruby 的内置方法
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

非常方便。

Ruby 的 `.capitalize` 方法很便利，但它只会将第一个字母变为大写。如果你想要更多的控制，或者想要将字符串中的每个词首字母都大写（这被称为标题样式），你可能想要使用 Rails ActiveSupport 扩展中的 `titleize` 方法，或者自己实现它：

```ruby
# 使用 Rails 中 ActiveSupport 的 'titleize'
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# 自制解决方案
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

这个方法将字符串分割成一个单词数组，将每个单词首字母大写，然后用空格将它们重新连接起来。

就个人而言，我在我的代码中将这个想法推进得更远。我编写了我自己的[`titleize` 方法，它考虑到了像 "a" 和 "the" 这样的小词](https://github.com/public-law/law_string/blob/master/lib/law_string.rb)。
