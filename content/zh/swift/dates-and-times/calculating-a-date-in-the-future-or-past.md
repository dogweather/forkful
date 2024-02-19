---
aliases:
- /zh/swift/calculating-a-date-in-the-future-or-past/
date: 2024-01-20 17:32:08.020058-07:00
description: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u5C31\u662F\
  \u786E\u5B9A\u4E00\u4E2A\u76F8\u5BF9\u4E8E\u5F53\u524D\u65E5\u671F\u4E4B\u524D\u6216\
  \u4E4B\u540E\u7684\u786E\u5207\u65E5\u671F\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8FD9\
  \u79CD\u8BA1\u7B97\u4EE5\u5904\u7406\u4E8B\u4EF6\u9884\u5B9A\u3001\u6570\u636E\u5B58\
  \u6863\u6216\u4EFB\u4F55\u9700\u8981\u65F6\u95F4\u5DEE\u5F02\u7684\u4EFB\u52A1\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.453830
model: gpt-4-1106-preview
summary: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u5C31\u662F\
  \u786E\u5B9A\u4E00\u4E2A\u76F8\u5BF9\u4E8E\u5F53\u524D\u65E5\u671F\u4E4B\u524D\u6216\
  \u4E4B\u540E\u7684\u786E\u5207\u65E5\u671F\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8FD9\
  \u79CD\u8BA1\u7B97\u4EE5\u5904\u7406\u4E8B\u4EF6\u9884\u5B9A\u3001\u6570\u636E\u5B58\
  \u6863\u6216\u4EFB\u4F55\u9700\u8981\u65F6\u95F4\u5DEE\u5F02\u7684\u4EFB\u52A1\u3002"
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
---

{{< edit_this_page >}}

## 何为与为何？
计算未来或过去的日期就是确定一个相对于当前日期之前或之后的确切日期。程序员进行这种计算以处理事件预定、数据存档或任何需要时间差异的任务。

## 示例代码：

Swift 语言中使用 `Date`, `Calendar`, 和 `DateComponents` 类实现日期计算。

```Swift
import Foundation

// 获取现在的日期
let now = Date()

// 使用当前用户默认日历
let calendar = Calendar.current

// 计算未来日期（比如5天后）
if let fiveDaysLater = calendar.date(byAdding: .day, value: 5, to: now) {
    print("五天后的日期是：\(fiveDaysLater)")
}

// 计算过去日期（比如10天前）
if let tenDaysBefore = calendar.date(byAdding: .day, value: -10, to: now) {
    print("十天前的日期是：\(tenDaysBefore)")
}
```

可能的输出结果：
```
五天后的日期是：2023-04-15 14:23:00 +0000
十天前的日期是：2023-03-31 14:23:00 +0000
```

## 背景探究：
在 Swift 出现之前，Objective-C 是处理日期的首选；使用 `NSDate` 和 `NSCalendar`。随着 Swift 的发展，这些类被 `Date` 和 `Calendar` 所取代，它们提供更简洁的语法和更好的类型安全。

除了 Calendar 方案外，也可以使用第三方库，如 SwiftDate。这些库可能提供了更复杂的功能，例如支持不同的日历系统或者更精细的日期操作。

日期计算涉及时区和夏令时这样的实现细节，这可能会对结果产生影响。因此，在处理日期时，总是要考虑到 Locale 和 TimeZone。

## 相关资源：

- [Swift Documentation - Date](https://developer.apple.com/documentation/foundation/date)
- [Swift Documentation - Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [SwiftDate Library](https://github.com/malcommac/SwiftDate)
- [NSHipster article on Date and Time](https://nshipster.com/datecomponents/)
