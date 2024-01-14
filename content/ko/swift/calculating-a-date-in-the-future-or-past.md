---
title:                "Swift: 미래나 과거의 날짜 계산하기"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜
날짜를 미래나 과거로 계산하는 일에 대해 관심을 갖게 된 이유를 1-2 문장으로 설명합니다.

## 어떻게
코드 블록 내부에서 예제와 출력을 함께 보여주는 코딩 예제입니다.
```Swift
// 현재 날짜를 가져와서 이틀 후의 날짜를 계산하는 예제입니다.
let today = Date()
let futureDate = Calendar.current.date(byAdding: .day, value: 2, to: today)
print(futureDate)
// 결과: Optional(2021-10-29 06:12:37 +0000)
```

## 깊은 고민
미래나 과거로 날짜를 계산하는 것에 대해 더 깊이 들어가서 설명합니다. 이를 위해 `Calendar` 클래스의 `date(byAdding:value:to:)` 메서드가 어떻게 동작하는지 집중적으로 알아봅니다. 또한, 다른 날짜 계산 방법들을 소개합니다.

## 또 다른 정보
Korean DevTools Community의 다른 유용한 링크들입니다.
- [Date 계산하기, Swift 4, 경제 Xcode](https://www.youtube.com/watch?v=U6Ksrvagyx8)
- [Swift의 날짜, 시간, Calendar, Date의 모든 것](https://flee.tistory.com/41)
- [Swift로 시간 다루기: Date와 Timezone](https://babbab2.tistory.com/20)

## 관련 링크
Korean DevTools Community의 다른 유용한 링크들입니다.
- [Date 계산하기, Swift 4, 경제 Xcode](https://www.youtube.com/watch?v=U6Ksrvagyx8)
- [Swift의 날짜, 시간, Calendar, Date의 모든 것](https://flee.tistory.com/41)
- [Swift로 시간 다루기: Date와 Timezone](https://babbab2.tistory.com/20)