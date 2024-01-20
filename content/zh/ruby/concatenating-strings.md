---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么和为什么？
**字符串的连接** 是将两个或更多的字符串组合成一个较大的字符串。程序员之所以这么做，是因为它允许他们从小块文本中创建复杄的信息或接受用户输入。

## 怎么做？
连接字符串有很多方式，我们将介绍两种常用的方法：操作符 `+` 和 `<<` ，以及 `concat` 方法。

#### 使用 `+` 和 `<<` 操作符
```Ruby
str1 = "你好，"
str2 = "世界！"

result = str1 + str2
puts result  # => "你好，世界！"

str1 << str2
puts str1  # => "你好，世界！"
```
#### 使用 `concat` 方法
```Ruby
str1 = "你好，"
str2 = "世界！"

str1.concat(str2)
puts str1  # => "你好，世界！"
```
## 深度探讨
数十年前，字符串连接在古老的B编程语言中就已经存在。在Ruby语言中我们有多种加强版实现。

除了上述方法，Ruby还提供 `join` 方法用于拼接数组中的所有字符串。

```Ruby
str_array = ["你好，", "世界！"]
puts str_array.join  # => "你好，世界！"
```
`+` 和 `concat` 会创建新的字符串对象，但 `<<` 和 `concat` 方法则会直接在原字符串对象上进行操作。这有助于在内存管理方面提高程序性能。

## 另请参阅
更多有关于Ruby字符串连接和其他相关主题的信息，可以参考以下链接：

- [Ruby中的字符串连接](https://www.rubyguides.com/2018/06/ruby-string-concatenation/)
- [Ruby中的字符串方法文档](https://ruby-doc.org/core-2.7.0/String.html)
- [Ruby中关于性能的话题](https://www.fastruby.io/blog/ruby/performance/a-faster-hash.html)