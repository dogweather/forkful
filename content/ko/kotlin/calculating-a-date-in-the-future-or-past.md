---
title:                "Kotlin: 미래나 과거의 날짜 계산하기"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜

날짜를 미래나 과거로 계산하는 것의 필요성에 대해 간단히 설명합니다.

일부 상황에서는 미래나 과거의 날짜를 계산하는 것이 필요할 수 있습니다. 예를 들어 마감일이나 이벤트 날짜를 미리 계산할 때가 있습니다. 또는 생일이나 기념일을 너무 늦지 않게 미리 계산하고 싶을 때도 있죠. 

# 어떻게

```Kotlin
fun calculateDate(year: Int, month: Int, day: Int, daysToAdd: Int) : LocalDate {
    val date = LocalDate.of(year, month, day)
    return date.plusDays(daysToAdd.toLong())
}
```

위의 예시 코드는 지정한 일 수(daysToAdd)만큼 미래의 날짜를 계산하는 방법을 보여줍니다. 이외에도 LocalDate 클래스를 활용하여 날짜를 계산할 수 있습니다. 코틀린의 DateTime 라이브러리는 미래나 과거의 날짜 계산에 매우 유용합니다.

위의 코드를 실행하면 아래와 같은 결과가 나옵니다.

```
2021-09-30
```

# 딥 다이브

지정한 날짜를 기준으로 미래나 과거의 날짜를 계산하는 방법은 다양합니다. 아래의 링크들을 참고하면 좀 더 딥하게 날짜 계산에 대해 알아볼 수 있습니다.

# 또 다른 참고자료
- https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/
- https://kotlinlang.org/docs/datetime.html
- https://kotlinlang.org/docs/dates-and-times.html