---
title:                "Kotlin: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜: 날짜를 문자열로 변환하는 것에 참여하는 이유는 무엇일까요?
날짜 데이터를 문자열 형태로 변환하는 것은 많은 개발자들이 흔히 하는 작업입니다. 대부분의 프로그래밍 언어에서 날짜를 문자열로 변환하는 방법이 제공되며, 이러한 기능은 프로그램에서 날짜 데이터를 다루는 데 유용합니다. 코틀린에서도 마찬가지입니다. 따라서 코틀린을 사용하는 개발자라면 날짜를 문자열로 변환하는 방법을 익혀두는 것이 필수적입니다.

## 하는 방법:
코틀린에서는 ```toString()``` 메서드를 사용하여 날짜를 문자열로 변환할 수 있습니다. 이 메서드는 임의의 형식으로 날짜를 문자열로 변환하는 기능을 제공합니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다.

```Kotlin
val date = LocalDate.of(2021, 9, 10)
val formattedDate = date.toString()
println(formattedDate) // 2021-09-10
```

위의 코드에서는 ```LocalDate``` 타입의 변수를 사용하여 날짜를 생성하고, ```toString()``` 메서드를 이용하여 해당 날짜를 문자열로 변환하였습니다. 출력 결과는 입력한 날짜와 동일한 형식의 문자열로 나타납니다.

이외에도 날짜를 원하는 형식으로 변환할 수 있는 다양한 메서드가 존재합니다. 예를 들면 ```format()``` 메서드를 사용하여 날짜를 지정한 형식대로 변환할 수도 있습니다. 또한, 날짜와 시간을 함께 표시하기 위해 ```LocalDateTime``` 타입을 사용할 수도 있습니다.

## 깊이 들어가기:
날짜를 문자열로 변환하는 작업은 우리가 생각하는 것보다 조금 더 복잡한 과정을 거칩니다. 실제로는 기본적으로 프로그래밍 언어에서 제공하는 기능을 이용하여 날짜 데이터를 문자열로 변환합니다. 이러한 기능을 쉽게 이용할 수 있도록 코틀린에서는 날짜 및 시간 관련 라이브러리를 제공하며, 이러한 라이브러리의 사용법을 알게 되는 것은 매우 중요합니다.

## 참고자료:
- [코틀린 공식 홈페이지의 Date and Time](https://kotlinlang.org/docs/datetime.html)
- [예제로 배우는 Kotlin: Date and Time](https://www.baeldung.com/kotlin-datetime)
- [Kotlin Tutorials: Date & Time](https://www.jetbrains.com/help/idea/datetime.html#date-time_related_tutorials)