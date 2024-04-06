---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:01.362395-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Swift\u7684`Foundation`\u6846\u67B6\u63D0\
  \u4F9B\u4E86`Date`\u7C7B\uFF0C\u4F7F\u5F97\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u548C\
  \u65F6\u95F4\u53D8\u5F97\u7B80\u5355\u660E\u4E86\u3002\u8FD9\u91CC\u662F\u4E00\u4E2A\
  \u5982\u4F55\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u7684\u57FA\u672C\u793A\u4F8B\uFF1A\
  ."
lastmod: '2024-04-05T21:53:48.458920-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
