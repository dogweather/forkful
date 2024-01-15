---
title:                "두 날짜 비교하기"
html_title:           "Kotlin: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜 비교를 하는 이유는 무엇일까요? 간단하게 말하자면, 여러분이 두 날짜를 비교하면서 시간 차이나 기간을 계산할 수 있기 때문입니다. 예를 들어, 어떤 일이 발생한 날짜와 오늘 날짜를 비교하여 그 사이에 얼마나 시간이 지났는지를 알 수 있습니다.

## 예제로 살펴보는 날짜 비교 방법

이제 Kotlin을 사용하여 두 날짜를 비교하는 방법에 대해 알아보겠습니다. 먼저 두 날짜 변수를 선언하고 각각의 대한 값들을 할당해야 합니다. 그리고 나서 날짜를 비교하는 코드를 작성해보겠습니다.

```Kotlin
// 두 날짜 변수 선언
val firstDate = LocalDateTime.of(2021, Month.JANUARY, 1, 0, 0, 0)
val secondDate = LocalDateTime.of(2021, Month.DECEMBER, 31, 23, 59, 59)

// 날짜 비교 코드
if(firstDate.isBefore(secondDate)) { // firstDate가 secondDate보다 이전인지 확인
    val diff = ChronoUnit.DAYS.between(firstDate, secondDate) // firstDate와 secondDate 사이에 몇 일 차이가 있는지 계산
    println("두 날짜 사이에는 $diff 일 차이가 있습니다.")
} else {
    println("날짜를 다시 입력해주세요.")
}
```

위의 코드를 실행하면 "두 날짜 사이에는 365 일 차이가 있습니다." 라는 출력 결과가 나올 것입니다.

## 날짜 비교의 깊은 이해

날짜를 비교하는 방법은 간단하지만, 내부적으로는 아주 복잡한 로직으로 이루어져 있습니다. 때때로 우리는 단순히 날짜 비교를 위해서만 사용하지만, 실제로는 시간의 분, 초까지 고려하는 경우도 있습니다. 따라서 날짜 비교를 할 때는 어떤 시간 단위를 사용하는지에 따라 결과가 달라질 수 있음을 기억해야 합니다.

## 더 많은 정보를 원하신다면

더 많은 정보를 원하신다면 아래 링크들을 참고하시기 바랍니다.

[Java documentation on LocalDate](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDateTime.html)

[Kotlin documentation on LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-kotlin.-local-date-time/)