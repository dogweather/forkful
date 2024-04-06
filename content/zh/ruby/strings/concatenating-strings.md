---
date: 2024-01-20 17:35:36.795448-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5B57\u7B26\u4E32\u62FC\u63A5\
  \u5728\u7F16\u7A0B\u8BED\u8A00\u7684\u65E9\u671F\u5C31\u6210\u4E3A\u4E86\u57FA\u7840\
  \u529F\u80FD\u4E4B\u4E00\u3002\u5728 Ruby \u4E2D\uFF0C\u5F00\u53D1\u8005\u7ECF\u5E38\
  \u7528\u5B83\u6765\u521B\u5EFA\u52A8\u6001\u5B57\u7B26\u4E32\u6216\u7A0B\u5E8F\u8F93\
  \u51FA\u3002\u9664\u4E86\u4E0A\u9762\u7684\u65B9\u6CD5\uFF0C`+`\u3001`<<`\u3001\
  `concat` \u4EE5\u53CA\u63D2\u503C\u4E4B\u5916\uFF0C\u8FD8\u53EF\u4EE5\u4F7F\u7528\
  \ `join` \u65B9\u6CD5\u5408\u5E76\u6570\u7EC4\u4E2D\u7684\u5B57\u7B26\u4E32\u3002\
  \ 1. `+` \u521B\u5EFA\u65B0\u5B57\u7B26\u4E32\uFF0C\u4F46\u6548\u7387\u4E0D\u5982\
  \ `<<`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.501052-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5B57\u7B26\u4E32\u62FC\u63A5\u5728\u7F16\
  \u7A0B\u8BED\u8A00\u7684\u65E9\u671F\u5C31\u6210\u4E3A\u4E86\u57FA\u7840\u529F\u80FD\
  \u4E4B\u4E00\u3002\u5728 Ruby \u4E2D\uFF0C\u5F00\u53D1\u8005\u7ECF\u5E38\u7528\u5B83\
  \u6765\u521B\u5EFA\u52A8\u6001\u5B57\u7B26\u4E32\u6216\u7A0B\u5E8F\u8F93\u51FA\u3002\
  \u9664\u4E86\u4E0A\u9762\u7684\u65B9\u6CD5\uFF0C`+`\u3001`<<`\u3001`concat` \u4EE5\
  \u53CA\u63D2\u503C\u4E4B\u5916\uFF0C\u8FD8\u53EF\u4EE5\u4F7F\u7528 `join` \u65B9\
  \u6CD5\u5408\u5E76\u6570\u7EC4\u4E2D\u7684\u5B57\u7B26\u4E32\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

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
