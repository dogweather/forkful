---
date: 2024-01-20 17:32:14.866700-07:00
description: "\uB0A0\uC9DC \uACC4\uC0B0\uC774\uB780 \uD604\uC7AC\uB85C\uBD80\uD130\
  \ \uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uD2B9\uC815 \uB0A0\uC9DC\uB97C \uC815\uD558\
  \uB294 \uAC78 \uB9D0\uD569\uB2C8\uB2E4. \uC571\uC5D0\uC11C \uD68C\uC6D0\uAC00\uC785\
  \ \uD6C4 30\uC77C\uC758 \uCCB4\uD5D8 \uAE30\uAC04\uC744 \uC124\uC815\uD558\uB294\
  \ \uB4F1, \uAE30\uAC04\uC744 \uC81C\uD55C\uD558\uAC70\uB098 \uC608\uC57D \uC2DC\uC2A4\
  \uD15C\uC744 \uAD6C\uCD95\uD560 \uB54C \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774\
  \ \uC8FC\uB85C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.750386-06:00'
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC \uACC4\uC0B0\uC774\uB780 \uD604\uC7AC\uB85C\uBD80\uD130 \uBBF8\
  \uB798\uB098 \uACFC\uAC70\uC758 \uD2B9\uC815 \uB0A0\uC9DC\uB97C \uC815\uD558\uB294\
  \ \uAC78 \uB9D0\uD569\uB2C8\uB2E4. \uC571\uC5D0\uC11C \uD68C\uC6D0\uAC00\uC785 \uD6C4\
  \ 30\uC77C\uC758 \uCCB4\uD5D8 \uAE30\uAC04\uC744 \uC124\uC815\uD558\uB294 \uB4F1\
  , \uAE30\uAC04\uC744 \uC81C\uD55C\uD558\uAC70\uB098 \uC608\uC57D \uC2DC\uC2A4\uD15C\
  \uC744 \uAD6C\uCD95\uD560 \uB54C \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC8FC\
  \uB85C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
weight: 26
---

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
