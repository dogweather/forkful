---
title:    "Swift: 두 날짜 비교하기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜?

무엇보다도, 유저나 프로그래머 모두 날짜와 시간에 대해 생각할 일이 많습니다. 예를 들어서, 유저가 북미에서 개최되는 온라인 이벤트에 참여하려면 그 이벤트 시작 시간과 유저의 현재 시간을 비교할 수 있어야 합니다.

또는, 프로그래머가 예약 시스템을 만들고 있다면, 오늘 날짜와 비교하여 예약이 가능한 날짜를 찾아야 할 것입니다. 이처럼 날짜 비교는 우리 생활과 소프트웨어 개발에 항상 쓰이는 것이니, 날짜를 비교하는 방법을 알고 있으면 큰 도움이 될 것입니다.

## 어떻게?

Swift에서 두 날짜를 비교하는 방법은 간단합니다. 먼저, 비교하고 싶은 두 날짜를 `Date` 객체로 만들어주어야 합니다. 그리고 두 날짜 중 어느 날짜가 더 미래인지 비교해야 할 때는 `>` 기호를 사용하면 됩니다. 반대로 어느 날짜가 과거인지 비교해야 할 때는 `<` 기호를 사용합니다.

```Swift
// 비교할 날짜를 원하는 형식으로 만든 후, Date 객체로 변환
let date1 = "2020-06-15".toDate()
let date2 = "2020-06-20".toDate()

// 두 날짜 비교
if date1 > date2 {
    print("\(date1.toString())은(는) \(date2.toString())보다 미래입니다.")
} else if date1 < date2 {
    print("\(date1.toString())은(는) \(date2.toString())보다 과거입니다.")
} else {
    print("\(date1.toString())은(는) \(date2.toString())와(과) 같은 날짜입니다.")
}

// 출력: 2020년 6월 15일은 2020년 6월 20일보다 과거입니다.
```

## 더 깊게

날짜를 비교할 때 유의해야 할 점은 시간대(time zone)와 날짜 포맷(date format)을 고려하는 것입니다. 예를 들어, 서로 다른 시간대에서 동일한 날짜를 비교할 수도 있고, 날짜의 문자열 표기 방식이 다르면 제대로 된 비교가 이루어지지 않을 수도 있습니다.

이에 대한 해결책으로는 먼저, `Calendar` 클래스를 사용하여 시간대 정보를 추가해주는 것이 도움이 됩니다. 또한, `DateFormatter` 클래스를 사용하여 날짜 포맷을 표준화해주면 더 정확한 비교가 가능합니다.

## 더 알아보기

- [Swift의 Date 클래스 문서](https://developer.apple.com/documentation/foundation/date)
- [Swift의 Calendar 클래스 문서](https://developer.apple.com/documentation/foundation/calendar)
- [Swift의 DateFormatter 클래스 문서](https://developer.apple.com/documentation/foundation/dateformatter)
- [날짜 비교 관련 스택 오버플로우 질문과 답변](https://stackoverflow.com/questions/35700281/how-do-i-compare-2-dates-in-swift)
- [시간대와 날짜 포맷 관련 스택 오버플로우 질문과 답변](https://stackoverflow.com/questions/39917564/is-the-date-format-about-time-zones-required-or-important-for-date-compli)

## 관련 링크

- [Swift와 날