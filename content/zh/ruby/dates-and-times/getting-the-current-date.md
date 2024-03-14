---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:40.673749-07:00
description: "\u5728\u51E0\u4E4E\u6240\u6709\u7F16\u7A0B\u5DE5\u4F5C\u4E2D\uFF0C\u83B7\
  \u53D6\u5F53\u524D\u65E5\u671F\u662F\u4E00\u4E2A\u57FA\u672C\u4EFB\u52A1\uFF0C\u4ECE\
  \u5E94\u7528\u7A0B\u5E8F\u4E2D\u8BB0\u5F55\u6D3B\u52A8\u5230\u751F\u6210\u5E26\u6709\
  \u65E5\u671F\u6233\u7684\u62A5\u544A\u3002\u5728Ruby\u4E2D\uFF0C\u53EF\u4EE5\u901A\
  \u8FC7\u4F7F\u7528\u6807\u51C6\u5E93\u8F7B\u677E\u5B8C\u6210\uFF0C\u7B80\u5316\u6D89\
  \u53CA\u65E5\u671F\u7684\u64CD\u4F5C\u3002"
lastmod: '2024-03-13T22:44:48.385591-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u51E0\u4E4E\u6240\u6709\u7F16\u7A0B\u5DE5\u4F5C\u4E2D\uFF0C\u83B7\
  \u53D6\u5F53\u524D\u65E5\u671F\u662F\u4E00\u4E2A\u57FA\u672C\u4EFB\u52A1\uFF0C\u4ECE\
  \u5E94\u7528\u7A0B\u5E8F\u4E2D\u8BB0\u5F55\u6D3B\u52A8\u5230\u751F\u6210\u5E26\u6709\
  \u65E5\u671F\u6233\u7684\u62A5\u544A\u3002\u5728Ruby\u4E2D\uFF0C\u53EF\u4EE5\u901A\
  \u8FC7\u4F7F\u7528\u6807\u51C6\u5E93\u8F7B\u677E\u5B8C\u6210\uFF0C\u7B80\u5316\u6D89\
  \u53CA\u65E5\u671F\u7684\u64CD\u4F5C\u3002"
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
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
