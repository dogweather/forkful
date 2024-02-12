---
title:                "두 날짜 비교하기"
date:                  2024-01-20T17:34:12.301273-07:00
model:                 gpt-4-1106-preview
simple_title:         "두 날짜 비교하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
(무엇과 왜?)
날짜 비교는 두 'Date' 인스턴스가 언제인지 확인하는 과정입니다. 시간관련 로직 처리, 이벤트 순서 결정, 데드라인 검사 등에 활용됩니다.

## How to:
(어떻게:)
Swift에서 날짜 비교를 하는 간단한 예제입니다:

```Swift
import Foundation

// 오늘 날짜와 비교할 특정 날짜 생성
let now = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd HH:mm"
let someDate = dateFormatter.date(from: "2023/05/01 08:00")!

// 날짜 비교
if now < someDate {
    print("아직 미래입니다.")
} else if now > someDate {
    print("이미 지난 날짜입니다.")
} else {
    print("지금입니다!")
}

// 날짜 간의 간격 계산
let interval = now.timeIntervalSince(someDate)
print("초 단위로 계산된 간격: \(interval)")
```

예상된 출력 결과:
```
아직 미래입니다.
초 단위로 계산된 간격: -숫자
```
(실제 출력되는 '숫자'는 코드를 실행하는 시점에 따라 달라집니다.)

## Deep Dive:
(심해 탐험)
Swift의 'Date'는 시간과 날짜를 다루는 핵심 클래스입니다. 'Date' 객체는 특정 시점을 '1970년 1월 1일 00:00:00 UTC'로부터의 초단위로 표현합니다. 대부분의 날짜 연산은 'TimeInterval' (초 단위)를 사용합니다.

만약 'Date'보다 더 복잡한 날짜 계산이 필요하다면, 'Calendar'를 활용하세요. 그것은 시간대, 로케일 등을 고려한 다양한 날짜 연산을 지원합니다.

날짜 비교에는 '<', '>', '==', '!=' 등의 연산자를 활용할 수 있으며, 'Date'와 'TimeInterval' 조합으로 날짜를 더하거나 뺄 수도 있습니다.

## See Also:
(추가 정보)
- Swift Documentation: [Date](https://developer.apple.com/documentation/foundation/date)
- Swift Documentation: [Calendar](https://developer.apple.com/documentation/foundation/calendar)
- Apple's NSDateFormatter Guide: [Date Formatting Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/DataFormatting.html)
