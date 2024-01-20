---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么&为什么？

日期到字符串的转换就是将日期格式转变为字符串格式。这样做的目的是为了方便程序员在显示、存储或作为参数传递时更容易地操作日期数据。

## 如何操作：

你可以用 `.strftime` 方法将日期转换成字符串。以下是一些示例，`Date.today` 为今天的日期。

```ruby
date = Date.today # 假设今天是 "2021-12-23"

# 转换为 "23/12/2021"
date.strftime("%d/%m/%Y") 
=> "23/12/2021"

# 转换为 "Dec 23, 2021"
date.strftime("%b %d, %Y") 
=> "Dec 23, 2021"
```
## 深入了解

在Ruby的历史发展中，`.to_s` 是早期版本的转换方法，但其不能支持日期的自定义格式化。而 `.strftime` 方法则支持各种格式化选项，使日期的呈现更具灵活性。

除此之外，还有 `.iso8601` 方法可以将日期转换为国际标准的日期字符串格式，如：`"2021-12-23"`。

实施这些转换的具体细节主要包括了字符串和日期的解析以及格式的应用。

```ruby
# 使用.iso8601方法
date.iso8601
=> "2021-12-23"
```

## 参见

你可能还需要查阅以下资源以便了解更多信息：

1.  Ruby 文档: [Date](https://ruby-doc.org/stdlib-3.1.0/libdoc/date/rdoc/Date.html)

2.  `.strftime` 方法的 [格式化选项列表](https://www.foragoodstrftime.com/)

3.  Ruby文档: [字符串格式化](https://ruby-doc.org/core-2.5.0/String.html#method-i-format)

请记住，每个问题都有多种解决方案，找到最适合你的那一个很重要。以上所有代码都是在最新版本的 Ruby （3.1.0）中运行的。