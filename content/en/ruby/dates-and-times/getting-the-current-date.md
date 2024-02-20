---
date: 2024-02-03 19:02:28.432475-07:00
description: "Fetching the current date is an essential task in almost any programming\
  \ endeavor, from logging activities in an application to generating reports with\u2026"
lastmod: 2024-02-19 22:05:19.022107
model: gpt-4-0125-preview
summary: "Fetching the current date is an essential task in almost any programming\
  \ endeavor, from logging activities in an application to generating reports with\u2026"
title: Getting the current date
---

{{< edit_this_page >}}

## What & Why?
Fetching the current date is an essential task in almost any programming endeavor, from logging activities in an application to generating reports with date stamps. In Ruby, this can be easily accomplished using the standard library, simplifying operations that involve dates.

## How to:
Ruby's standard library includes the `Date` and `Time` classes for handling dates and time. Here's how to get the current date:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

Sample output: 
```
2023-04-12
```

For including time with the date, Ruby's `Time` class is more suitable:

```ruby
current_time = Time.now
puts current_time
```

Sample output: 
```
2023-04-12 14:33:07 +0200
```

If you need more functionality, such as time zone management, you might want to use a third-party gem like `ActiveSupport` (part of Rails but can be used standalone).

First, add `activesupport` to your Gemfile and run `bundle install`:

```ruby
gem 'activesupport'
```

Then, use it to handle time zones:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # Set your desired time zone
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

Sample output:
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
