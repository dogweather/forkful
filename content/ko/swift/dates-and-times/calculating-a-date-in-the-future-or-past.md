---
date: 2024-01-20 17:32:14.866700-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.750386-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
weight: 26
---

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
