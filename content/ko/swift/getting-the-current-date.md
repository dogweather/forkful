---
title:                "현재 날짜 불러오기"
html_title:           "Swift: 현재 날짜 불러오기"
simple_title:         "현재 날짜 불러오기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 어떤 것이며 왜 필요한가요?
현재 날짜를 가져오는 것이란 무엇일까요? 그리고 왜 프로그래머들이 이것을 하는 걸까요? 현재 날짜를 가져오는 것은 프로그래밍에서 매우 유용한 작업입니다. 이는 어떤 기능이나 앱에서 사용 가능한 최신 정보를 제공하고, 시간과 일정을 추적하며, 데이터를 정렬할 때 도움을 줍니다.

## 방법:
```
// 현재 날짜를 가져오는 방법은 간단합니다. Date()를 호출하면 됩니다.
let currentDate = Date()
print(currentDate)
```
```
// 날짜를 원하는 형식으로 포맷하는 방법은 다음과 같습니다.
let format = DateFormatter()
format.dateFormat = "yyyy/MM/dd"
let formattedDate = format.string(from: currentDate)
print(formattedDate)
```
출력 예시:
```
2019-08-19 13:18:22 +0000
2019/08/19
```

## 깊이 있는 들어보기:
(1) 현재 날짜를 가져오는 방법에는 여러 가지가 있습니다. Swift의 Date() 외에도 Calendar, strftime() 등의 다양한 옵션이 있습니다. (2) 현재 날짜를 이용해 시계, 알람, 카운트다운 등 다양한 기능을 구현할 수 있습니다. (3) 현재 날짜를 효율적으로 관리하기 위해 Date와 관련된 다양한 함수와 메서드를 알아보세요. 예를 들어, 날짜 간의 차이를 구하는 방법, 날짜를 연산하는 방법 등이 있습니다.

## 관련 자료:
- [Date and Time Programming Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html#//apple_ref/doc/uid/10000039i)
- [DateFormatter - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [Alternative Ways to Get the Current Date and Time - Swift by Sundell](https://www.swiftbysundell.com/posts/alternative-ways-to-get-the-current-date-and-time-in-swift)