---
date: 2024-01-20 17:35:36.795448-07:00
description: "\u5B57\u7B26\u4E32\u62FC\u63A5\u662F\u5C06\u4E24\u4E2A\u6216\u66F4\u591A\
  \u7684\u5B57\u7B26\u4E32\u8FDE\u63A5\u8D77\u6765\u5F62\u6210\u4E00\u4E2A\u65B0\u5B57\
  \u7B26\u4E32\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u505A\u8FD9\u4E8B\u4E3B\u8981\
  \u662F\u4E3A\u4E86\u6784\u9020\u8F93\u51FA\u3001\u751F\u6210\u52A8\u6001\u5185\u5BB9\
  \u6216\u8005\u5728\u5C06\u6570\u636E\u5B58\u50A8\u5230\u6587\u4EF6\u6216\u6570\u636E\
  \u5E93\u4E4B\u524D\u8FDB\u884C\u683C\u5F0F\u5316\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:22.174242-06:00'
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u62FC\u63A5\u662F\u5C06\u4E24\u4E2A\u6216\u66F4\u591A\
  \u7684\u5B57\u7B26\u4E32\u8FDE\u63A5\u8D77\u6765\u5F62\u6210\u4E00\u4E2A\u65B0\u5B57\
  \u7B26\u4E32\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u505A\u8FD9\u4E8B\u4E3B\u8981\
  \u662F\u4E3A\u4E86\u6784\u9020\u8F93\u51FA\u3001\u751F\u6210\u52A8\u6001\u5185\u5BB9\
  \u6216\u8005\u5728\u5C06\u6570\u636E\u5B58\u50A8\u5230\u6587\u4EF6\u6216\u6570\u636E\
  \u5E93\u4E4B\u524D\u8FDB\u884C\u683C\u5F0F\u5316\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
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
