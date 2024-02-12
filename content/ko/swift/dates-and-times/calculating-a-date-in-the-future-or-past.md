---
title:                "미래나 과거의 날짜 계산하기"
aliases:
- /ko/swift/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:32:14.866700-07:00
model:                 gpt-4-1106-preview
simple_title:         "미래나 과거의 날짜 계산하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜 계산이란 현재로부터 미래나 과거의 특정 날짜를 정하는 걸 말합니다. 앱에서 회원가입 후 30일의 체험 기간을 설정하는 등, 기간을 제한하거나 예약 시스템을 구축할 때 프로그래머들이 주로 사용합니다.

## How to: (어떻게:)
```Swift
import Foundation

// 현재 날짜를 가져옵니다.
let now = Date()

// 캘린더를 설정합니다.
var calendar = Calendar.current

// 5일 후의 날짜를 계산합니다.
if let fiveDaysLater = calendar.date(byAdding: .day, value: 5, to: now) {
    print("5일 후: \(fiveDaysLater)")
}

// 3주 전 날짜를 계산합니다.
if let threeWeeksBefore = calendar.date(byAdding: .weekOfYear, value: -3, to: now) {
    print("3주 전: \(threeWeeksBefore)")
}
```
Sample Output:
```
5일 후: 2023-04-21 12:00:00 +0000
3주 전: 2023-03-10 12:00:00 +0000
```

## Deep Dive (심화 탐구)
날짜 계산은 그레고리력을 기반으로, 컴퓨터 기술이 발달하면서 중요해졌습니다. NSDate와 같은 초기 클래스에서 시작해 어느새 더욱 합리적이고 정교해진 Date 구조체로 발전했죠. 대안적으로 `TimeInterval`을 사용해 날짜를 계산할 수도 있습니다만, 그것은 시간의 간격을 초 단위로만 다룬다는 한계가 있습니다. 반면, Calendar API를 사용하면 더 자연스러운 날짜 단위로 작업할 수 있습니다. TimeZone, Locale 설정을 고려하여 국제화된 앱을 만들 때도 유리합니다.

## See Also (관련 자료)
- [Swift Documentation: Date](https://developer.apple.com/documentation/foundation/date)
