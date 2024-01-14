---
title:                "Kotlin: 현재 날짜 얻기"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜 날짜를 가져와야 하는가?

날짜와 시간은 우리 일상생활에서 중요한 역할을 합니다. 가장 기본적으로는 우리가 언제 일정을 잡아야 하는지, 언제 약속을 잡아야 하는지를 알려주지만 더 중요한 것은 문서작성, 데이터 정렬 등 다양한 분야에서 사용될 수 있습니다. 코틀린에서는 매우 간단하게 현재 날짜와 시간을 가져오는 방법을 배울 수 있습니다.

## 가져오는 방법

가장 간단한 방법은 'java.time' 패키지에서 제공하는 'LocalDateTime' 클래스를 사용하는 것입니다. 다음과 같이 코드를 작성해보세요.

```Kotlin
import java.time.LocalDateTime

val currentDateTime = LocalDateTime.now()
println("Current Date and Time: $currentDateTime")
```

위 코드를 실행하면 다음과 같은 출력이 나옵니다.

```
Current Date and Time: 2021-09-29T13:25:31.911459
```

또 다른 방법으로는 'java.util' 패키지에서 제공하는 'Date' 클래스를 사용하는 것입니다. 다음과 같이 코드를 작성해보세요.

```Kotlin
import java.util.Date

val currentDate = Date()
println("Current Date: $currentDate")
```

이 코드를 실행하면 다음과 같은 출력이 나옵니다.

```
Current Date: Wed Sep 29 13:27:08 KST 2021
```

두 가지 방법 모두 간단하게 현재 날짜와 시간을 가져올 수 있지만, 'LocalDateTime' 클래스를 사용하는 것이 더 권장됩니다. 이유는 'Date' 클래스는 JDK 1.0부터 사용되던 클래스로, 많은 버그와 성능 이슈가 있기 때문입니다.

## 더 깊게 알아보기

'LocalDateTime' 클래스는 년도, 월, 일, 시간, 분, 초, 밀리초 등 다양한 정보를 제공합니다. 위에서 우리는 'now()' 메서드를 사용하여 현재 날짜와 시간을 가져왔지만 이외에도 다양한 메서드를 사용할 수 있습니다. 

예를 들어, 'LocalDateTime.now()' 대신 'of()' 메서드를 사용하면 특정 날짜와 시간을 지정하여 가져올 수 있고, 'plus' 메서드를 사용하면 특정 시간을 더하거나 빼는 등 다양한 작업을 할 수 있습니다.

더 자세한 내용은 공식 문서를 참고해보시기 바랍니다.

## 관련 링크

- [코틀린 공식 문서](https://kotlinlang.org/docs/datetime.html)
- [Java 8 날짜와 시간 API 소개](https://www.baeldung.com/java-8-date-time-intro)
- [Date 클래스의 문제점](https://www.baeldung.com/java-date-class-problems)