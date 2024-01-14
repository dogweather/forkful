---
title:                "Kotlin: 두 날짜 비교하기"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜 비교를 하는 이유는 무엇일까요? 

두 날짜를 비교하면 두 날짜 사이의 시간 차이를 계산할 수 있습니다. 이는 프로그래밍에서 매우 유용한 기능이며, 예를 들어 파산 날짜와 현재 날짜 사이의 남은 시간을 계산하는 데 사용할 수 있습니다.

## 어떻게

날짜를 비교하는 방법에 대해 알아보겠습니다. Kotlin 언어를 사용하여 다음과 같이 두 날짜를 비교할 수 있습니다.

```Kotlin
val formatter = SimpleDateFormat("yyyy-MM-dd") // 비교할 날짜 형식 설정
val date1 = formatter.parse("2021-10-10") // 비교할 첫 번째 날짜
val date2 = formatter.parse("2021-12-25") // 비교할 두 번째 날짜

// 두 날짜 비교
if (date1.before(date2)) {
    println("date2가 더 늦은 날짜입니다.")
} else if (date2.before(date1)) {
    println("date1이 더 늦은 날짜입니다.")
} else {
    println("두 날짜는 같은 날짜입니다.")
}
```

위 코드를 실행하면 `date2가 더 늦은 날짜입니다.`라는 출력 결과를 볼 수 있습니다.

## 깊게 들어가기

위 예제에서 사용된 `SimpleDateFormat`은 날짜 형식을 문자열과 일치시키는 데 유용한 클래스입니다. 이 클래스 외에도 `Calendar` 클래스를 사용하여 날짜 간의 시간 차이를 계산할 수 있습니다. 또한 `LocalDate`와 같은 새로운 날짜/시간 API를 사용하여 더욱 쉽게 날짜를 비교할 수 있습니다.

## 더 알아보기

더 많은 정보를 원한다면 아래 링크들을 확인해보세요.

- [Kotlin 공식 문서 - 날짜/시간 API](https://kotlinlang.org/docs/datetime-overview.html)
- [날짜 형식 지정 문자열 참고](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Java Date vs. Calendar](https://www.geeksforgeeks.org/java-date-vs-calendar-class/) 

## 연관된 항목

- [Kotlin으로 미래 날짜 예약하기](https://linktofutureblogpost.com)
- [Calendar 클래스를 사용한 시간 변경하기](https://linktochangeblogpost.com)