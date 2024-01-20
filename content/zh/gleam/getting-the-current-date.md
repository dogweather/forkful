---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么和为什么？
获取当前日期是检索和使用现在的日期和时间的过程。程序员之所以这样做，是因为它可以帮助他们进行时间记录，比如跟踪用户活动、生成日志或协调事件。

## 如何操作：
在Gleam中，我们可以使用标准库接口`datetime`的`now`函数来获取当前日期和时间。以下是如何实现的代码：

```Gleam
import gleam/datetime
let now = datetime.now()
```

这将会生成像这样的输出：

```Gleam
#Datetime(.year=2022, .month=March, .day=26, .hour=5, .minute=30, .second=0, .millisecond=0, .zone=Utc)
```

## 深度解析
### 历史背景
从1980年代早期开始，计算机程序就开始获取和利用当前日期。这有各种各样的应用，比如计算年龄、时间段等。

### 替代方案
如果你只需要时间而不需要日期，你可以使用`Time.now`，如果你只需要日期，你可以使用`Date.today`。请注意，所有这些函数都返回UTC时间。

### 实现细节
在Gleam中，日期和时间是使用ISO 8601格式表示的，这也是为什么你在输出中看到一个带有`Utc`的`zone`字段。

## 参见