---
date: 2024-01-20 17:32:14.866700-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) \uB0A0\uC9DC \uACC4\uC0B0\uC740 \uADF8\
  \uB808\uACE0\uB9AC\uB825\uC744 \uAE30\uBC18\uC73C\uB85C, \uCEF4\uD4E8\uD130 \uAE30\
  \uC220\uC774 \uBC1C\uB2EC\uD558\uBA74\uC11C \uC911\uC694\uD574\uC84C\uC2B5\uB2C8\
  \uB2E4. NSDate\uC640 \uAC19\uC740 \uCD08\uAE30 \uD074\uB798\uC2A4\uC5D0\uC11C \uC2DC\
  \uC791\uD574 \uC5B4\uB290\uC0C8 \uB354\uC6B1 \uD569\uB9AC\uC801\uC774\uACE0 \uC815\
  \uAD50\uD574\uC9C4 Date \uAD6C\uC870\uCCB4\uB85C \uBC1C\uC804\uD588\uC8E0. \uB300\
  \uC548\uC801\uC73C\uB85C `TimeInterval`\uC744 \uC0AC\uC6A9\uD574 \uB0A0\uC9DC\uB97C\
  \ \uACC4\uC0B0\uD560 \uC218\uB3C4\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.979943-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:) \uB0A0\uC9DC \uACC4\uC0B0\uC740 \uADF8\uB808\uACE0\
  \uB9AC\uB825\uC744 \uAE30\uBC18\uC73C\uB85C, \uCEF4\uD4E8\uD130 \uAE30\uC220\uC774\
  \ \uBC1C\uB2EC\uD558\uBA74\uC11C \uC911\uC694\uD574\uC84C\uC2B5\uB2C8\uB2E4."
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
