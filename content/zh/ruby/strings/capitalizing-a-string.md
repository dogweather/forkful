---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:12.534608-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5927\u5199\u5316\u4E00\u4E2A\u5B57\u7B26\
  \u4E32\u901A\u5E38\u662F\u6307\u5C06\u4E00\u4E2A\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\
  \u4E2A\u5B57\u7B26\u8F6C\u6362\u4E3A\u5927\u5199\uFF0C\u5176\u4F59\u7684\u8F6C\u4E3A\
  \u5C0F\u5199\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u51FA\u4E8E\u9075\u5FAA\
  \u547D\u540D\u89C4\u8303\u3001\u4F7F\u8F93\u51FA\u66F4\u6613\u4E8E\u9605\u8BFB\u6216\
  \u786E\u4FDD\u6570\u636E\u4E00\u81F4\u6027\u4EE5\u7528\u4E8E\u6BD4\u8F83\u548C\u5B58\
  \u50A8\u7B49\u539F\u56E0\u3002"
lastmod: '2024-03-11T00:14:22.164175-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5927\u5199\u5316\u4E00\u4E2A\u5B57\u7B26\
  \u4E32\u901A\u5E38\u662F\u6307\u5C06\u4E00\u4E2A\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\
  \u4E2A\u5B57\u7B26\u8F6C\u6362\u4E3A\u5927\u5199\uFF0C\u5176\u4F59\u7684\u8F6C\u4E3A\
  \u5C0F\u5199\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u51FA\u4E8E\u9075\u5FAA\
  \u547D\u540D\u89C4\u8303\u3001\u4F7F\u8F93\u51FA\u66F4\u6613\u4E8E\u9605\u8BFB\u6216\
  \u786E\u4FDD\u6570\u636E\u4E00\u81F4\u6027\u4EE5\u7528\u4E8E\u6BD4\u8F83\u548C\u5B58\
  \u50A8\u7B49\u539F\u56E0\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
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
