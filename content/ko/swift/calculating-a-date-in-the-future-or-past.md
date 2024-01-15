---
title:                "미래나 과거 날짜 계산하기"
html_title:           "Swift: 미래나 과거 날짜 계산하기"
simple_title:         "미래나 과거 날짜 계산하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것은 일상 생활에서 매우 많이 사용되는 기능입니다. 예를 들어, 언제 다음 생일이나 휴가를 가는 날짜가 되는지 알고 싶을 때 유용하게 사용할 수 있습니다.

## 사용 방법

날짜를 미래나 과거로 계산하는 방법은 매우 간단합니다. 우선 `Date()` 함수를 사용하여 현재 시간을 얻은 다음 `Calendar`와 `DateComponents`를 이용하여 미래나 과거로 계산할 날짜를 정의합니다. 그리고 `.date(byAdding: DateComponents, to: Date)` 메소드를 사용하여 계산된 날짜를 얻을 수 있습니다. 아래 예시 코드를 참고하세요.

```Swift
let today = Date() // 현재 날짜
let calendar = Calendar.current // 현재의 달력을 사용
var dateComponents = DateComponents() // 날짜 계산에 사용할 DateComponents 객체 생성
dateComponents.year = 1 // 1년 이후 날짜 계산
let nextYear = calendar.date(byAdding: dateComponents, to: today) // 계산된 날짜를 변수에 저장
print(nextYear) // 1년 이후 날짜 출력
```

출력 결과는 아래와 같이 나타납니다.

```
Optional(2019-07-22 10:00:00 +0000)
```

위와 같은 방식으로 원하는 날짜를 계산할 수 있습니다.

## Deep Dive

날짜를 계산할 때에는 `Calendar`와 `DateComponents` 객체가 필수적으로 사용됩니다. `Calendar` 객체는 시스템의 달력을 나타내며, `DateComponents` 객체는 날짜 계산에 필요한 단위들을 담고 있습니다. 일반적으로 `Calendar.current`를 사용하면 시스템의 달력을 바로 사용할 수 있습니다. 또한 `DateComponents`는 연도, 달, 일자, 시간, 분, 초 등 날짜의 다양한 단위들을 설정할 수 있습니다.

## See Also

- [Swift 공식 문서](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID341)
- [Swift 프로그래밍: 초기 단계](https://dubin-lee.github.io/2019/03/28/swift_01/)