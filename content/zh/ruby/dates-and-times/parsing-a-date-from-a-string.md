---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:38.001444-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u5728Ruby\u4E2D\uFF0C\u6807\u51C6\u5E93\u63D0\
  \u4F9B\u4E86\u76F4\u63A5\u4F7F\u7528`Date`\u548C`DateTime`\u7C7B\u4ECE\u5B57\u7B26\
  \u4E32\u89E3\u6790\u65E5\u671F\u7684\u65B9\u5F0F\u3002\u4EE5\u4E0B\u662F\u4F7F\u7528\
  Ruby\u5185\u7F6E\u65B9\u6CD5\u8FDB\u884C\u64CD\u4F5C\u7684\u65B9\u5F0F\uFF1A."
lastmod: '2024-03-13T22:44:48.384373-06:00'
model: gpt-4-0125-preview
summary: "\u5728Ruby\u4E2D\uFF0C\u6807\u51C6\u5E93\u63D0\u4F9B\u4E86\u76F4\u63A5\u4F7F\
  \u7528`Date`\u548C`DateTime`\u7C7B\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\
  \u7684\u65B9\u5F0F\u3002\u4EE5\u4E0B\u662F\u4F7F\u7528Ruby\u5185\u7F6E\u65B9\u6CD5\
  \u8FDB\u884C\u64CD\u4F5C\u7684\u65B9\u5F0F\uFF1A."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

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
