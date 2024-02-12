---
title:                "获取当前日期"
date:                  2024-02-03T19:11:01.362395-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
在Swift中获取当前日期涉及使用`Date`类来访问应用程序正在运行时的日期和时间。程序员需要获取当前日期的原因有很多，范围从时间戳事件，执行日期计算，到在用户界面显示日期和时间。

## 如何操作：
Swift的`Foundation`框架提供了`Date`类，使得获取当前日期和时间变得简单明了。这里是一个如何获取当前日期的基本示例：

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

这将输出类似于：

```
2023-04-12 07:46:23 +0000
```

输出格式遵循ISO 8601标准，并使用UTC时区。然而，您可能想要将这个日期格式化以供显示。Swift的`DateFormatter`类能够帮助您：

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

样例输出可能是：

```
April 12, 2023 at 10:46:23 AM
```

注意，输出格式将根据运行代码的设备的区域设置而变化。

对于需要更复杂日期操作的项目，许多Swift开发人员转向使用第三方库，如`SwiftDate`。以下是您可能使用`SwiftDate`在特定时区和格式下获取当前日期的方式：

首先，使用SPM、CocoaPods或Carthage将`SwiftDate`添加到您的项目中。然后：

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

这可能会输出：

```
2023-04-12 09:46:23
```

使用`SwiftDate`，您可以轻松地为不同的时区和区域设置操作日期和时间，简化您的Swift应用程序中复杂的日期处理任务。
