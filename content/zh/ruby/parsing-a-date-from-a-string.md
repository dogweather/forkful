---
title:                "从字符串解析日期"
date:                  2024-02-03T19:15:38.001444-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串解析日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
