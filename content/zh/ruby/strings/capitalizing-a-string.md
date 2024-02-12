---
title:                "字符串大写化"
aliases:
- /zh/ruby/capitalizing-a-string.md
date:                  2024-02-03T19:06:12.534608-07:00
model:                 gpt-4-0125-preview
simple_title:         "字符串大写化"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
在编程中，大写化一个字符串通常是指将一个字符串的第一个字符转换为大写，其余的转为小写。程序员这样做是出于遵循命名规范、使输出更易于阅读或确保数据一致性以用于比较和存储等原因。

## 如何做：
Ruby 提供了直接的方法用于字符串操作，包括大写化。这里是如何在 Ruby 中大写化一个字符串的方法：

```ruby
# Ruby 的内置方法
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Ruby 的 `.capitalize` 方法很方便，但只影响第一个字母。如果您想要更多的控制，或想要将字符串中的每个单词大写化（被称为标题样式），您可能会想使用 Rails ActiveSupport 扩展中的 `titleize` 方法，或自己实现它：

```ruby
# 在 Rails 中使用 ActiveSupport 的 'titleize'
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

如果您不使用 Rails 或更喜欢纯 Ruby 解决方案，这里是如何大写化字符串中的每一个单词：

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

这个方法将字符串分割成一个单词数组，大写化每一个，然后用空格将它们连接回一起。
