---
title:                "Swift: 두 날짜 비교하기"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜

날짜를 비교하는 것이 왜 중요한지 이해하기 위해 우리는 종종 다양한 날짜와 시간을 다루는 프로그램을 만들어야 합니다. 이러한 프로그램이 어떻게 작동하는지 알아보기 위해 날짜 비교를 배우는 것은 매우 중요합니다. 

# 어떻게

날짜를 비교하는 방법에는 여러 가지가 있습니다. 먼저, Swift의 ```Date``` 클래스를 사용하여 날짜를 만들고 비교할 수 있습니다. 예를 들어, 다음 코드를 사용하여 오늘의 날짜와 내일의 날짜를 비교할 수 있습니다.

```Swift
let today = Date()
let tomorrow = Calendar.current.date(byAdding: .day, value: 1, to: today)
```

그리고 우리는 간단한 조건문을 사용하여 두 날짜를 비교할 수 있습니다.

```Swift
if today < tomorrow {
    print("내일이 오늘보다 더 미래입니다.")
} else if today > tomorrow {
    print("오늘이 내일보다 더 미래입니다.")
} else {
    print("오늘과 내일은 같은 날짜입니다.")
}
```

결과는 다음과 같이 출력됩니다.

```
내일이 오늘보다 더 미래입니다.
```

# 더 깊은 내용

날짜를 비교할 때, 시작 날짜와 종료 날짜 사이의 시간 간격도 비교할 수 있습니다. 이를 위해 Swift의 ```DateInterval``` 클래스를 사용할 수 있습니다. 다음은 두 날짜 사이의 시간 간격을 비교하는 간단한 예제 코드입니다.

```Swift
let startDate = Date()
let endDate = Date().addingTimeInterval(60)

let interval = DateInterval(start: startDate, end: endDate)

if interval.duration > 0 {
    print("시작 날짜와 종료 날짜 사이의 시간 간격은 \(interval.duration)초 입니다.")
}
```

출력 결과는 다음과 같이 나타납니다.

```
시작 날짜와 종료 날짜 사이의 시간 간격은 60.0초 입니다.
```

## 봐도 좋은 것들

- [Swift "Date and Time Programming Guide"](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html#//apple_ref/doc/uid/10000039i)
- [Swift Date and Time Tutorial: Getting Started with Dates in Swift](https://www.raywenderlich.com/4581-swift-date-and-time-getting-started-with-dates-in-swift)
- [Swift Date Cheat Sheet](https://learnappmaking.com/date-swift-cheat-sheet-how-to-ios-developer-swiftui-ios13-dateformatter/)