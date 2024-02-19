---
aliases:
- /zh/ruby/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:38.001444-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u89E3\u6790\u65E5\u671F\uFF0C\u662F\u6307\
  \u5C06\u4EE3\u8868\u65E5\u671F\u7684\u6587\u672C\u8F6C\u6362\u4E3ARuby\u80FD\u7406\
  \u89E3\u7684`Date`\u6216`DateTime`\u5BF9\u8C61\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u6267\u884C\u6BD4\u8F83\u3001\u8BA1\u7B97\u6216\u683C\u5F0F\
  \u5316\u65E5\u671F\u7B49\u64CD\u4F5C\uFF0C\u8FD9\u5728\u5904\u7406\u8C03\u5EA6\u3001\
  \u5206\u6790\u6216\u6570\u636E\u5904\u7406\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u662F\
  \u5E38\u89C1\u4EFB\u52A1\u3002"
lastmod: 2024-02-18 23:08:59.609125
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u89E3\u6790\u65E5\u671F\uFF0C\u662F\u6307\
  \u5C06\u4EE3\u8868\u65E5\u671F\u7684\u6587\u672C\u8F6C\u6362\u4E3ARuby\u80FD\u7406\
  \u89E3\u7684`Date`\u6216`DateTime`\u5BF9\u8C61\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u6267\u884C\u6BD4\u8F83\u3001\u8BA1\u7B97\u6216\u683C\u5F0F\
  \u5316\u65E5\u671F\u7B49\u64CD\u4F5C\uFF0C\u8FD9\u5728\u5904\u7406\u8C03\u5EA6\u3001\
  \u5206\u6790\u6216\u6570\u636E\u5904\u7406\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u662F\
  \u5E38\u89C1\u4EFB\u52A1\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么?
从字符串中解析日期，是指将代表日期的文本转换为Ruby能理解的`Date`或`DateTime`对象。程序员这样做是为了执行比较、计算或格式化日期等操作，这在处理调度、分析或数据处理的应用程序中是常见任务。

## 如何操作:
在Ruby中，标准库提供了直接使用`Date`和`DateTime`类从字符串解析日期的方式。以下是使用Ruby内置方法进行操作的方式：

```ruby
require 'date'

# 从字符串解析日期
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# DateTime来表示更详细的时间
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

为了更多的控制或处理`parse`可能直接不理解的格式，你可以使用`strptime`（字符串解析时间），明确指定格式：

```ruby
# 使用strptime进行自定义格式
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### 使用第三方库:

虽然Ruby的内置功能强大，但有时你可能更喜欢使用第三方库以获得额外的功能或更简单的语法。一个流行的选择是用于自然语言解析的`Chronic`宝石库：

1. 首先，在你的Gemfile中添加Chronic并运行`bundle install`:
```ruby
gem 'chronic'
```

2. 然后，这样使用它：
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('next Tuesday')
puts parsed_chronic
# 输出会根据当前日期变化；假定在2023-04-01解析
# => 2023-04-04 12:00:00 +0000
```

`Chronic`用于用户输入非常有用，因为它能理解广泛的自然语言日期格式，使其成为需要灵活日期输入的应用程序的强大工具。
