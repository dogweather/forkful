---
title:                "Kotlin: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜?

날짜를 과거나 미래로 계산하는 것이 왜 중요할까요?

## 왜

여러분이 내일은 무슨 요일인지, 아니면 다음달에 어떤 날짜이고, 혹은 10년 후에는 무슨 해나 달인지 알고 싶다면 날짜를 계산해야합니다. 예를 들어, 특정 이벤트를 계획할 때 날짜를 정확하게 계산하는 것이 중요할 수 있습니다.

## 방법

날짜를 계산하려면 Kotlin의 내장 함수인 `Calendar`와 `SimpleDateFormat`을 사용할 수 있습니다. 아래는 예시 코드입니다.

```Kotlin
val calendar = Calendar.getInstance()
calendar.add(Calendar.DAY_OF_YEAR, 1) // 내일의 날짜를 계산합니다.
val dateFormat = SimpleDateFormat("yyyy/MM/dd")
val tomorrow = dateFormat.format(calendar.time)
println("내일의 날짜는 $tomorrow 입니다.")
```
**출력: 내일의 날짜는 2019/05/07 입니다.**

위의 코드는 `Calendar` 객체를 사용하여 현재 날짜를 기준으로 내일의 날짜를 계산하는 방법을 보여줍니다.

## 깊게 들어가보기

`Calendar` 클래스를 사용하면 날짜를 쉽게 계산할 수 있습니다. 이 클래스를 사용하려면 `getInstance()`를 호출하여 객체를 초기화해야 합니다. 또한, `Calendar`는 날짜와 시간을 모두 다룰 수 있으며, 원하는 포맷으로 출력할 수 있도록 `SimpleDateFormat` 클래스를 함께 사용하는 것이 좋습니다.

시간대와 관련된 문제를 해결하기 위해 `java.util.TimeZone`을 사용하면 좋고, 또한 날짜 계산의 정확성을 위해 `java.time` 패키지의 클래스를 사용할 수 있습니다.

# 더 알아보기

- [Java Calendaring Basics](https://docs.oracle.com/javase/tutorial/datetime/overview/calendar.html)
- [Working with dates and times in Java 8](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- [Kotlin DateTime API](http://www.kotlinlang.org/docs/reference/datetime.html)