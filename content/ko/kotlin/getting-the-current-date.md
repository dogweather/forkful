---
title:                "현재 날짜 받기"
html_title:           "Kotlin: 현재 날짜 받기"
simple_title:         "현재 날짜 받기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇이며, 왜? 
현재 날짜를 얻는 것은 프로그래머들이 쉽게 프로그램 내에서 날짜를 추적하고 사용할 수 있도록 도와주는 기능입니다. 프로그래머들은 주로 이 기능을 사용하여 시간 기반 로직을 구현하거나, 프로그램에서 특정 날짜를 출력하거나 저장할 때 사용합니다.

## 사용 방법:
```Kotlin
val currentDate = Date()
```
위의 예시 코드에서는 'Date'라는 내장 클래스를 사용하여 현재 날짜를 변수에 할당하는 방법을 보여줍니다. 이렇게하면 변수 'currentDate'에 현재 날짜와 시간의 정보가 들어갑니다. 

```Kotlin 
val currentTime = LocalDateTime.now()
```
또 다른 예시 코드에서는 'LocalDateTime'라는 내장 클래스를 사용하여 더 세밀한 날짜 정보를 얻는 방법을 보여줍니다. 이 클래스는 현재 날짜뿐만 아니라 연도, 월, 일, 시간, 분, 초 등의 세부 정보를 제공합니다.

## 더 깊이 들어가보기:
- 이 기능은 Java 1.0 버전부터 지원되었으며, 여러 언어나 프레임워크에서도 사용할 수 있는 표준 기능입니다.
- 위의 예시 코드에서 'Date' 클래스를 사용한 방법은 더 이상 권장되지 않습니다. 'LocalDateTime' 클래스를 사용할 것을 권장합니다.
- 이 기능을 활용하여 앱의 기기별 다른 시간대를 처리하는 것도 가능합니다.

## 관련 자료:
- [Kotlin 언어 공식 문서](https://kotlinlang.org/docs/tutorials/datetime.html)- 현재 날짜와 시간을 다루는 다양한 방법이 자세하게 설명되어 있습니다.
- [Java 내장 'Date' 클래스](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)- Java 언어로 처음 발표된 날짜와 시간을 다루는 클래스입니다.