---
title:                "Swift: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

여러분은 날짜를 미래나 과거로 계산하는 것에 참여할 이유가 있을까요? 예를 들어, 어떤 특정한 날짜로부터 100일 후가 언제인지 알아야 할 수도 있을 것입니다. 이 블로그 포스트에서는 스위프트로 쉽게 날짜를 계산하는 방법을 알려드리겠습니다.

## 방법

다음은 스위프트로 날짜를 계산하는 간단한 예제 코드입니다. 아래 코드를 보고 따라해보세요.

```Swift
let now = Date()
let futureDate = Calendar.current.date(byAdding: .day, value: 100, to: now)
print(futureDate)
```

위 코드는 현재 날짜에서 100일을 더한 날짜를 계산하고 출력합니다. 이렇게 계산한 날짜는 Date 형식으로 출력되며, 이를 필요에 따라 원하는 형식으로 변환할 수 있습니다.

## 깊게 파고들기

날짜를 계산하는 과정은 Date, Calendar, DateFormatter 등의 프레임워크를 사용하여 이루어집니다. Date는 초 단위로 측정된 시간 정보를 담고 있으며, Calendar는 날짜와 시간을 계산하는 기능을 제공합니다. DateFormatter는 날짜 형식을 지정하여 원하는 형태로 출력하는 역할을 합니다.

더 자세한 내용은 스위프트 문서를 참고하시기 바랍니다.

## 관련 링크

- 스위프트 공식 문서: https://developer.apple.com/documentation/swift/
- 날짜 계산 예제: https://www.hackingwithswift.com/example-code/system/how-to-calculate-the-number-of-days-between-two-dates
- DateFormatter 사용 방법: https://www.nsdateformatter.com/
- 스위프트로 날짜와 시간 다루기: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html