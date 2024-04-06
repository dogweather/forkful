---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:40.673749-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Ruby\u7684\u6807\u51C6\u5E93\u5305\u62EC\
  \u7528\u4E8E\u5904\u7406\u65E5\u671F\u548C\u65F6\u95F4\u7684`Date`\u548C`Time`\u7C7B\
  \u3002\u4EE5\u4E0B\u662F\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u7684\u65B9\u6CD5\uFF1A\
  ."
lastmod: '2024-04-05T22:38:47.520161-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Ruby\u7684\u6807\u51C6\u5E93\u5305\u62EC\u7528\
  \u4E8E\u5904\u7406\u65E5\u671F\u548C\u65F6\u95F4\u7684`Date`\u548C`Time`\u7C7B\u3002\
  \u4EE5\u4E0B\u662F\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u7684\u65B9\u6CD5\uFF1A."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
