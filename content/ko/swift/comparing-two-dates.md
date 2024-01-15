---
title:                "두 날짜 비교하기"
html_title:           "Swift: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
날짜를 비교하는 것이 왜 유용한지 알고 싶으신가요? 예를 들어, 사용자의 생일을 기준으로 나이를 계산하거나, 이전과 이후의 날짜를 비교해 이벤트의 빈도를 분석할 수 있습니다.

## 방법
두 날짜를 비교하려면, 먼저 `Date` 형식의 변수를 생성해야 합니다. 그리고 다음과 같은 메서드를 사용하면 됩니다:
```Swift
let date1 = Date()
let date2 = Date.init(timeIntervalSinceNow: -86400) //24시간 전의 날짜
if date1 < date2 {
    print("Date 1 is before date 2")
} else if date2 < date1 {
    print("Date 2 is before date 1")
} else {
    print("The dates are equal")
}
```
> "Date 2 is before date 1"이 출력될 것입니다.

## 깊게 들어가보기
날짜를 비교할 때 고려해야 할 몇 가지 중요한 점들이 있습니다. 첫째로, 날짜는 `TimeInterval` 형태로 저장되며, 이는 초 단위로 표현됩니다. 따라서 두 날짜를 비교하기 전에, 같은 형식으로 날짜를 변환해주어야 합니다. 또한, `Date`는 시스템의 현재 날짜와 시간을 기준으로 하기 때문에, 시간이나 시간대를 고려할 필요가 있습니다. 이러한 고려를 통해 정확한 날짜 비교를 할 수 있습니다.

## 더 알아보기
날짜를 다루는 더 많은 기능들을 알아보려면 [Apple의 공식 문서](https://developer.apple.com/documentation/foundation/date)를 참고하세요.

## 관련 링크
- [Swift 날짜 비교 방법](https://medium.com/swift-programming/swift-how-to-compare-dates-39a46090fb8c)
- [Date Formatter 사용하기](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID299)
- [Swift의 날짜와 시간 다루기](https://www.avanderlee.com/swift/datecomponents-dateformatters/)