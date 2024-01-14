---
title:    "Kotlin: 현재 날짜 가져오기"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## 왜
*왜 누군가가 현재 날짜를 받아야 하는지에 대한 이해*

현재 날짜를 받는 것은 프로그램에서 매우 중요한 기능입니다. 프로그램이 실행될 때마다 항상 다른 날짜를 사용해야할 때가 있고, 사용자의 시스템 시간에 따라 다른 결과를 출력해야할 때가 있습니다. 또한 프로그램에서 날짜를 사용하여 다양한 통계 또는 서비스를 제공하는데에도 매우 유용합니다. 고정된 날짜를 사용하는 대신에 항상 최신의 날짜를 받는다면, 더 정확하고 신뢰도 높은 프로그램을 만들 수 있습니다.

## 방법
*```Kotlin ``` 코드 블록과 함께 코딩 예제와 출력 결과*

먼저, `java.time.LocalDate` 클래스를 사용하여 현재 날짜를 받아올 수 있습니다. 예를 들어, 다음과 같은 코드를 사용할 수 있습니다:

```Kotlin
val today = LocalDate.now()
println(today)
```

위 코드의 출력 결과는 `2021-09-10`와 같이 오늘 날짜가 출력될 것입니다.

또 다른 방법으로는 `java.time.format.DateTimeFormatter` 클래스를 사용하여 날짜를 원하는 형식으로 포맷팅할 수 있습니다. 예를 들어, 다음과 같은 코드를 사용하여 현재 날짜를 "YYYY-MM-dd" 형식으로 출력할 수 있습니다.

```Kotlin
val today = LocalDate.now()
val formatter = DateTimeFormatter.ofPattern("YYYY-MM-dd")
val formattedDate = today.format(formatter)
println(formattedDate)
```

위 코드의 출력 결과는 `2021-09-10`가 될 것입니다.

## 심층 탐구
*현재 날짜를 받아오는 더 깊은 정보*

날짜를 받아오는 방법은 다양하게 존재하며, 각각의 방법마다 장단점이 존재합니다. `LocalDate.now()`를 사용하는 경우, 현재 시스템의 시간대에 따라 날짜가 달라질 수 있습니다. 또한, 시간대가 변경되면 결과가 변경될 수 있습니다. 따라서, 애플리케이션이 여러 지역에서 사용되는 경우 정확한 결과를 얻기 위해서는 추가적인 설정이 필요할 수 있습니다.

또한, 날짜를 포맷팅하는 방법에도 여러가지가 있습니다. `DateTimeFormatter` 클래스 외에도 `SimpleDateFormat` 클래스를 사용할 수도 있습니다. 각각의 방식은 다양한 포맷 옵션을 제공하지만, `DateTimeFormatter`가 더욱 객체 지향적인 설계를 제공하며 더욱 유연한 사용이 가능합니다.

## 관련 자료
*Kotlin에서 현재 날짜를 받는 방법에 대한 관련 자료들*

- [Kotlin Docs - LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)
- [Kotlin Docs - DateTimeFormatter](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-date-time-formatter/)
- [Java Docs - LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Docs - SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)