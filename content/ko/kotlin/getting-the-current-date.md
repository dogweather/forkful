---
title:                "현재 날짜 가져오기"
html_title:           "Kotlin: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 얻는 과정에 참여하는 이유는 매우 간단합니다. 날짜는 우리 일상 생활에서 매우 중요한 역할을 하며, 많은 프로그래밍 작업에서도 필수적으로 사용됩니다. 따라서 개발자들은 현재 날짜를 얻는 방법을 알고 있어야 합니다.

## 어떻게

현재 날짜를 얻는 방법은 Kotlin에서 매우 간단합니다. 다음 예제 코드를 참고해보세요.

```Kotlin
val date = LocalDate.now()
println(date)
```
출력 결과:
```
2019-10-10
```

위의 코드를 보면 `LocalDate` 클래스의 `now()` 메서드를 호출하여 현재 날짜를 얻어서 변수에 저장한 뒤, `println` 함수를 사용하여 출력하고 있습니다. 즉, `LocalDate` 클래스를 사용하면 현재 날짜를 쉽게 얻을 수 있습니다.

## 딥 다이브

Kotlin에서 현재 날짜를 다루는 `LocalDate` 클래스는 `java.time` 패키지에서 제공하는 표준 라이브러리입니다. 이 클래스는 오늘 날짜를 쉽게 얻을 수는 있지만, 다른 날짜나 시간 정보를 다루는 것도 가능합니다. 예를 들어, 다음과 같은 코드를 이용하면 현재 시간 정보도 얻을 수 있습니다.

```Kotlin
val dateTime = LocalDateTime.now()
println(dateTime)
```

출력 결과:
```
2019-10-10T16:22:20.056
```

또한 여러 가지 날짜/시간 포맷으로 출력하는 것도 가능합니다. `DateTimeFormatter` 클래스를 사용하여 원하는 포맷의 문자열로 변환할 수 있습니다. 예를 들어, 다음 코드는 현재 날짜를 `yyyy년 MM월 dd일` 포맷으로 출력합니다.

```Kotlin
val formatter = DateTimeFormatter.ofPattern("yyyy년 MM월 dd일")
val formattedDate = formatter.format(date)
println(formattedDate)
```

출력 결과:
```
2019년 10월 10일
```

이외에도 `LocalDate` 클래스는 날짜의 연산이나 비교도 가능하며, 더 많은 기능을 제공합니다. 자세한 내용은 [공식 문서](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)를 참고하시기 바랍니다.

## 관련 링크

- [Kotlin 문서 - LocalDate Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/)
- [Java 문서 - LocalDate Class](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Kotlin Tutorial - Date and Time](https://www.tutorialspoint.com/kotlin/kotlin_date_time.htm)

## 참고

이제 Kotlin에서 현재 날짜를 얻는 방법을 알게 되었으니, 여러분의 프로그램에 적용해보세요!