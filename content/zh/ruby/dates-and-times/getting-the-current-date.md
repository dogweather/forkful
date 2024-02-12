---
title:                "获取当前日期"
aliases: - /zh/ruby/getting-the-current-date.md
date:                  2024-02-03T19:10:40.673749-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
在几乎所有编程工作中，获取当前日期是一个基本任务，从应用程序中记录活动到生成带有日期戳的报告。在Ruby中，可以通过使用标准库轻松完成，简化涉及日期的操作。

## 如何操作：
Ruby的标准库包括用于处理日期和时间的`Date`和`Time`类。以下是获取当前日期的方法：

```ruby
require 'date'

current_date = Date.today
puts current_date
```

示例输出：
```
2023-04-12
```

如果需要包括日期和时间，Ruby的`Time`类更适合：

```ruby
current_time = Time.now
puts current_time
```

示例输出：
```
2023-04-12 14:33:07 +0200
```

如果你需要更多功能，比如时区管理，你可能想使用第三方宝石（gem）`ActiveSupport`（Rails的一部分，但可以单独使用）。

首先，将`activesupport`添加到你的Gemfile并运行`bundle install`：

```ruby
gem 'activesupport'
```

然后，使用它来处理时区：

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # 设置你想要的时区
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

示例输出：
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
