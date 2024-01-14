---
title:                "Swift: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

为什么要比较两个日期：

了解两个日期之间的差异可以帮助我们更好地管理时间，并且能够帮助我们确定特定日期的相对位置。

如何比较两个日期：

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"

//Example dates
let date1 = dateFormatter.date(from: "2020-10-10")
let date2 = dateFormatter.date(from: "2020-10-15")

//Comparing dates using comparison operators
if date1 < date2 {
  print("Date1 is before Date2")
} else if date1 > date2 {
  print("Date1 is after Date2")
} else {
  print("Both dates are the same")
}
```

输出：
> Date1 is before Date2

深入了解比较两个日期：

比较两个日期时，我们需要确保两个日期具有相同的格式，才能准确比较。在Swift中，通过 DateFormatter 类来帮助我们解析字符串并将其转换为日期对象。通过使用 `dateFormat` 属性，我们可以指定日期格式，从而确保比较过程准确无误。

另外，还可以使用 `compare()` 方法来比较两个日期，它会返回一个 `ComparisonResult` 值，分别代表两个日期的相对大小（`.orderedAscending`、`.orderedDescending`和`.orderedSame`）。

参考链接：

- [Swift DateFormatter Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift Comparison Operators](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#ID27)
- [Swift compare() Method Documentation](https://developer.apple.com/documentation/foundation/nsdate/1408479-compare)

## 参考链接：

- [Swift DateFormatter文档](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift比较运算符](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#ID27)
- [Swift compare()方法文档](https://developer.apple.com/documentation/foundation/nsdate/1408479-compare)