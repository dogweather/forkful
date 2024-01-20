---
title:                "计算未来或过去的日期"
html_title:           "Ruby: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什麼與為什麼？

計算未來或過去的日期是通過添加或減去特定的日期來獲取新的日期。這在處理項目時段，事件計劃或存儲信息等程式設計涉及時間運算的場景被廣泛使用。

## 如何操作：

以下是兩種獲取未來日期的範例。

```Ruby
require 'date'

t = Date.today    # 今日日期
future = t + 5   # 未來五天的日期
puts future
```
假設今天是2022年8月1日，輸出結果為：
```
2022-08-06
```
以下是一種獲取過去日期的範例。

```Ruby
require 'date'

t = Date.today  # 今日日期
past = t - 30  # 三十天前的日期
puts past
```
假設今天是2022年8月1日，輸出結果為：
```
2022-07-02
```

## 深入探索

歷史上，Ruby從早期版本就提供了Date類來處理日期。但在進行精確的時間運算時，會使用DateTime類，該類提供了更精確的時間單位。

較現代的替代選擇是ActiveSupport::TimeWithZone，它是Ruby on Rails框架的一部分，可以處理包括時區在內的更複雜的日期時間問題。

在Ruby的日期計算中，一個重要的實現細節是，日期的加減遵循了天然的日期規則，比如說考慮到了每月的不同天數甚至是閏年。

## 另請參見

- Ruby官方文檔的Date類介紹：[點擊這裡](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html)
- Ruby官方文檔的DateTime類介紹：[點擊這裡](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/DateTime.html)
- Ruby on Rails的ActiveSupport::TimeWithZone介紹：[點擊這裡](https://apidock.com/rails/ActiveSupport/TimeWithZone)