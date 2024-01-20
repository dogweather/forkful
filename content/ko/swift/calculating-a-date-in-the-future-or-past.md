---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "Swift: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Swift로 미래 혹은 과거 날짜 계산하기

## 무엇이고 왜?

때론, 앱에서 미래의 특정 날짜나 시간 혹은 과거의 날짜나 시간을 계산해야 할 필요가 있습니다. 예를 들어, 일정 관련 앱이나 카운트다운 타이머 같은 곳에서는 기능이 필수적입니다. 프로그래머들이 이 계산을 하는 이유는 이러한 기능을 구현하기 위함입니다.

## 어떻게 할까?

우리는 Swift로 이 작업을 수행하기 위해 `Date`, `Calendar` 및 `DateComponents`를 사용합니다. 예제를 보겠습니다.

```Swift
import Foundation
let calendar = Calendar.current
let now = Date()

if let futureDate = calendar.date(byAdding: .day, value: 30, to: now) {
    print("30일 후: \(futureDate)")
}

if let pastDate = calendar.date(byAdding: .day, value: -30, to: now) {
    print("30일 전: \(pastDate)")
}
```

출력은 다음과 같습니다:
```Swift
30일 후: 2021-10-22 11:58:23 +0000
30일 전: 2021-08-24 11:58:23 +0000
```

## 깊이 있는 내용

Swift의 `Date`, `Calendar`, `DateComponents` 클래스는 네XTSTEP API를 기반으로 합니다. Swift가 국제 표준 날짜 및 시간 계산을 지원하기 위해 개발된 이후, 이 클래스들은 표준에 따라 미래 또는 과거의 날짜를 계산함으로써, UTC, 표준 시간대, 서머 타임 등 복잡한 요소로 인해 발생하는 여러 문제를 해결합니다.

대체 방법으로, 사람들은 종종 86400을 곱하여 일수를 계산합니다 (24시간*60분*60초). 그러나 이러한 방법은 서머 시간이나 윤초로 인한 문제를 안고 있습니다.

일반적으로, `date(byAdding:value:to:)` 메서드를 사용하여 날짜를 계산하는 것이 가장 안전하고 믿을 수 있습니다.

## 참고 자료

- Swift의 공식 문서: [Date](https://developer.apple.com/documentation/foundation/date), [Calendar](https://developer.apple.com/documentation/foundation/calendar), [DateComponents](https://developer.apple.com/documentation/foundation/datecomponents)
- 영상 튜토리얼: [Swift Date & Time: Comparing Dates (Tutorial)](https://www.youtube.com/watch?v=jfAcLgINrVs)