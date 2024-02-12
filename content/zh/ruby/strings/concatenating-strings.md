---
title:                "字符串拼接"
aliases:
- /zh/ruby/concatenating-strings.md
date:                  2024-01-20T17:35:36.795448-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串拼接"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
字符串拼接是将两个或更多的字符串连接起来形成一个新字符串的过程。程序员做这事主要是为了构造输出、生成动态内容或者在将数据存储到文件或数据库之前进行格式化。

## How to: (如何操作：)
```Ruby
# 使用 '+' 拼接字符串
greeting = "你好, " + "世界!"
puts greeting # 输出：你好, 世界!

# 使用 '<<' 连接符，又称 "shovel operator"
food = "面"
food << "条"
puts food # 输出：面条

# 使用 'concat' 方法
weather = "天气"
weather.concat("预报")
puts weather # 输出：天气预报

# 使用字符串插值
user = "小明"
message = "早上好，#{user}!"
puts message # 输出：早上好，小明!
```

## Deep Dive (深入探讨)
字符串拼接在编程语言的早期就成为了基础功能之一。在 Ruby 中，开发者经常用它来创建动态字符串或程序输出。除了上面的方法，`+`、`<<`、`concat` 以及插值之外，还可以使用 `join` 方法合并数组中的字符串。

1. `+` 创建新字符串，但效率不如 `<<` 或 `concat`。
2. `<<` 和 `concat` 直接在原字符串上修改，不创建新字符串，效率较高。
3. 插值是一种更简洁、Ruby风格的拼接方式，可以自动调用 `to_s` 方法将非字符串转换为字符串。
4. `join` 相比于 `+`，在处理数组元素合并时更高效。

拼接大量字符串时，频繁使用 `+` 可能导致性能问题，因为每次都会创建新的字符串对象。相反，`<<` 和 `concat` 由于是在原地修改，更适合处理大型数据。

## See Also (另见)
- Ruby 文档中的字符串操作：[Ruby-Doc String](https://ruby-doc.org/core-2.7.0/String.html)
