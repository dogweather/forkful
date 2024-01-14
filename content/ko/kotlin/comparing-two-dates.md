---
title:    "Kotlin: 두 날짜 비교하기"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜
날짜를 두 개 비교하는 일이 어떤 이유 때문에 중요할까요? 일반적으로 날짜를 비교하는 것은 유용한 일일 수 있습니다. 예를 들어, 어떤 사건이 두 날짜 사이에 발생했는지 알고 싶을 때, 또는 특정 날짜 이후의 일정을 확인하고 싶을 때 등이 있을 수 있습니다.

# 어떻게
날짜를 비교하는 것은 매우 간단합니다. Kotlin에서는 `isAfter()`, `isBefore()`, `equals()`와 같은 메서드를 사용하여 두 날짜를 비교할 수 있습니다. 또한 `compareTo()` 메서드를 사용하여 두 날짜를 비교할 수 있습니다. 아래는 Kotlin으로 두 날짜를 비교하는 간단한 예제 코드입니다.

```
Kotlin
val date1 = LocalDate.of(2021, 1, 1)
val date2 = LocalDate.of(2021, 1, 15)

println(date1.isBefore(date2)) // Output: true
println(date1.compareTo(date2)) // Output: -14
```

# 깊이있는 탐구
아래는 `compareTo()` 메서드를 사용하여 날짜를 비교하는 방법에 대한 몇 가지 깊이있는 정보입니다.

- 두 날짜가 같으면 `compareTo()`는 0을 반환합니다.
- 첫 번째 날짜가 두 번째 날짜보다 이전이면 `compareTo()`는 음수를 반환합니다.
- 첫 번째 날짜가 두 번째 날짜보다 이후이면 `compareTo()`는 양수를 반환합니다.

더 자세한 정보는 Kotlin 공식 문서를 참고하시기 바랍니다.

# 참고자료
- [Kotlin 공식 문서](https://kotlinlang.org/docs/compare-multiple-items.html#comparison-methods)
- [Java 날짜 비교 방법](https://www.baeldung.com/java-compare-dates)
- [날짜와 시간 처리를 위한 Java 8 새로운 기능들](https://docs.oracle.com/javase/tutorial/datetime/overview/index.html])

# 더 보기