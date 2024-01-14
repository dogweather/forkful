---
title:    "Kotlin: 두 날짜를 비교하기"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## 왜
때때로 우리는 두 날짜를 비교해야 할 때가 있습니다. 예를 들어, 년도를 계산하거나 이전 날짜와 현재 날짜 사이의 경과 시간을 알고 싶을 때 등 말이죠. 이러한 일을 할 때 우리는 두 날짜가 얼마나 차이가 나는지 알아야 합니다. 코틀린에서 날짜를 비교하는 방법을 배워보세요.

## 어떻게
이제 우리는 코틀린을 사용하여 두 날짜를 비교하는 방법을 살펴보겠습니다. 아래의 예제 코드를 참고해주세요.

```Kotlin
val date1 = LocalDate.of(2021, 1, 1)
val date2 = LocalDate.of(2021, 6, 1)
println(date1.isAfter(date2)) // false
println(date1.isBefore(date2)) // true
```

위의 예제 코드에서 우리는 두 개의 `LocalDate` 변수를 생성했습니다. 그리고 `isAfter()`와 `isBefore()` 메소드를 사용하여 두 날짜를 비교했습니다. 결과적으로 첫 번째 날짜가 두 번째 날짜보다 앞에 있는지 여부를 확인할 수 있습니다.

또 다른 방법으로는 `compareTo()` 메소드를 사용하는 것도 있습니다. 아래의 예제 코드를 확인해주세요.

```Kotlin
val date1 = LocalDate.of(2021, 1, 1)
val date2 = LocalDate.of(2021, 6, 1)
println(date1.compareTo(date2)) // -1
println(date2.compareTo(date1)) // 1
```

`compareTo()` 메소드는 첫 번째 날짜가 두 번째 날짜와 비교했을 때 어떤 값을 반환하는지를 나타냅니다. 위의 예제에서 보시면 첫 번째 날짜가 두 번째 날짜보다 빠른 날짜이므로 `-1`이라는 값이 반환됩니다.

## 딥 다이브
우리는 지금까지 코틀린에서 두 날짜를 비교하는 방법을 살펴보았습니다. 하지만 이 외에도 `isEquals()` 메소드를 사용하여 두 날짜가 동일한 날짜인지 아닌지도 비교할 수 있습니다. 또한, `LocalDateTime`과 `ZonedDateTime`같은 다른 타입의 날짜와 시간을 비교하는 방법에 대해서도 알아보는 것이 좋습니다.

## 비슷한 글도 읽어보세요
- [Kotlin의 LocalDate 클래스](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)
- [Kotlin의 날짜 및 시간 관련 함수](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/index.html)