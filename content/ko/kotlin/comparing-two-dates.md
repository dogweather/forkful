---
title:                "Kotlin: 두 날짜를 비교하는 법"
simple_title:         "두 날짜를 비교하는 법"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
날짜 비교를 하는 이유는 무엇일까요? 날짜 비교는 일상 생활에서 자주 사용되는 중요한 작업입니다. 두 날짜를 비교하는 것은 예약 시스템에서 예약 가능한 날짜를 확인하거나 할인 이벤트 기간을 확인하는 등 여러 상황에서 필수적입니다.

## 하는 방법
날짜를 비교하는 방법은 다양합니다. 하지만 코틀린에서는 쉽고 간결하게 날짜를 비교할 수 있는 다양한 방법을 제공합니다. 아래의 코딩 예제를 통해 살펴보도록 하겠습니다.

```Kotlin 
// 두 날짜가 같은지 확인하기
val date1 = LocalDate.of(2021, 8, 1)
val date2 = LocalDate.of(2021, 8, 1)
println(date1 == date2) // true 출력

// 두 날짜가 이전 날짜인지 확인하기
val date1 = LocalDate.of(2021, 8, 1)
val date2 = LocalDate.of(2021, 6, 1)
println(date1.isBefore(date2)) // false 출력

// 두 날짜가 이후 날짜인지 확인하기
val date1 = LocalDate.of(2021, 8, 1)
val date2 = LocalDate.of(2021, 10, 1)
println(date1.isAfter(date2)) // false 출력
```

위의 코드에서 사용된 `LocalDate`는 코틀린의 날짜 클래스 중 하나로, JDK 8에서 추가된 클래스입니다. 날짜를 생성하고 비교하는 메소드(`isEqual()`, `isBefore()`, `isAfter()`)를 사용할 수 있으며, 두 날짜를 비교할 때 `==` 연산자를 사용할 수도 있습니다.

## 깊이있게 살펴보기
코틀린에서는 날짜 비교를 위한 다양한 클래스를 제공합니다. `LocalDate` 외에도 `LocalTime`, `LocalDateTime` 등의 클래스를 사용할 수 있습니다. 또한 `ZonedDateTime`을 사용하여 특정 시간대에 대한 날짜 비교를 할 수도 있습니다.

또한 날짜 비교를 할 때 주의해야 할 점이 있습니다. 우리가 사용하는 그레고리력은 윤년이 존재하기 때문에, 같은 날짜지만 윤년인지 아닌지에 따라 결과가 달라질 수 있습니다. 따라서 프로젝트에서 사용하는 달력을 반드시 확인하고, 날짜를 비교할 때 조심해야 합니다.

## 또 다른 정보들

### Kotlin Koans - Comparisons
https://play.kotlinlang.org/koans/overview

### The Kotlin Standard Library - Comparisons
https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.comparisons/


## 참고 자료
이 포스트에서 살펴본 내용 이외에도 다양한 방법으로 날짜를 비교할 수 있습니다. 아래의 링크들을 참고하면 더 많은 정보를 얻을 수 있습니다. 

Kotlin Cheat Sheet - date and time
https://kotlinlang.org/docs/dates.html

Kotlin - Date and Time 
https://www.baeldung.com/kotlin/dates