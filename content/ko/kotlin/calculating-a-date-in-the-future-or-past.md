---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Kotlin: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 일에 참여하는 이유는 매우 많습니다. 예를 들어, 휴가 계획을 세울 때나 중요한 일정을 조율할 때 등 날짜를 미리 계산해두는 것이 유용합니다.

## 하는 법

```Kotlin
// 현재 날짜를 가져 옵니다.
val currentDate = LocalDate.now()

// 1년 후의 날짜를 계산합니다.
val futureDate = currentDate.plus(Period.ofYears(1))

// 계산한 날짜를 출력합니다.
println("1년 후의 날짜는: $futureDate")
```

출력 결과:

```
1년 후의 날짜는: 2022-11-03
```

```Kotlin
// 현재 날짜를 가져 옵니다.
val currentDate = LocalDate.now()

// 3일 후의 날짜를 계산합니다.
val futureDate = currentDate.plus(Period.ofDays(3))

// 계산한 날짜를 출력합니다.
println("3일 후의 날짜는: $futureDate")
```

출력 결과:

```
3일 후의 날짜는: 2021-11-08
```

## 깊이 파헤치기

날짜를 미래나 과거로 계산할 때, 개발자가 알아야 하는 것은 `LocalDate`와 `Period` 객체입니다. `LocalDate`는 시스템의 현재 날짜를 반환하는 역할을 합니다. `Period`는 시간 간격을 나타내는 클래스로, `Period.ofYears()`나 `Period.ofDays()`와 같은 메서드를 통해 원하는 시간 단위를 지정할 수 있습니다. 또한, `plus()` 메서드를 통해 날짜 간의 연산을 수행할 수 있습니다.

## 참고 자료

- [Kotlin 공식 문서](https://kotlinlang.org/docs/home.html)
- [Java로 날짜 계산하기 - Java Time API](https://codechacha.com/ko/java-date-time/)
- [Kotlin으로 날짜 계산하기](https://perfectacle.github.io/2020/11/01/kotlin-date-time/)
- [Kotlin 기본 문법 공부용 미션 - 날짜 계산하기](https://blog.lerina.kr/ko/posts/kotlin-mission-date/)