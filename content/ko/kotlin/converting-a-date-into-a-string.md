---
title:                "날짜를 문자열로 변환하기"
html_title:           "Kotlin: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜를 문자열로 변환하는 것은 날짜 데이터를 사용하기 쉽게 하기 위한 프로그래밍 기술입니다. 프로그래머들이 이를 하는 이유는 우리가 일상에서 다루는 날짜 형식과 컴퓨터가 이해하는 형식 사이의 차이 때문입니다.

## 방법:

**예제 1:** `LocalDate` 객체를 생성한 후 `toString()` 메서드를 사용하여 문자열로 변환하는 방법:
```Kotlin
val date = LocalDate.parse("2021-01-01")
println(date.toString())
```
**결과:** `"2021-01-01"`

**예제 2:** 현재 날짜를 `Date` 객체로 가져와서 `SimpleDateFormat`을 사용하여 원하는 포맷으로 문자열로 변환하는 방법:
```Kotlin
val currentDate = Date()
val dateFormat = SimpleDateFormat("yyyy-MM-dd")
println(dateFormat.format(currentDate))
```
**결과:** `"2021-08-25"`

## 심층 분석:

(1) 날짜를 문자열로 변환하는 기술은 컴퓨터의 발전과 함께 시작되었습니다. 이전에는 컴퓨터가 날짜를 이해할 수 있는 형식으로 데이터를 입력해야 했기 때문에 이러한 기술은 필수적이었습니다. (2) 다른 대안으로는 날짜 형식을 변경하지 않고도 날짜 정보를 추출하는 것이 있지만, 이는 날짜 데이터의 가독성을 높여주는 이점이 있습니다. (3) 문자열로 변환하는 과정에서 잘못된 형식의 데이터가 입력되면 예외가 발생할 수 있기 때문에 오류 처리가 중요합니다.

## 관련 자료:

- [Kotlin Date and Time](https://kotlinlang.org/docs/datetime/)
- [Java Date and Time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [SimpleDateFormat JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)