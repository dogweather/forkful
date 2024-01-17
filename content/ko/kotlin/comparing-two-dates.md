---
title:                "두 날짜 비교하기"
html_title:           "Kotlin: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

두 개의 날짜를 비교하는 것은 무엇인가? 프로그래머들이 이를 하는 이유는 무엇인가?

## 무엇 & 왜?

날짜를 비교하는 것은 두 날짜를 비교하여 어느 날짜가 더 미래인지를 알아내는 것을 의미합니다. 프로그래머들은 이를 통해 예약 시스템, 유효 기간 체크 및 이벤트 일정 등 여러 가지 상황에서 유용하게 활용할 수 있습니다.

## 사용 방법:

### Kotlin에서 날짜 비교하기

날짜를 비교하는 가장 간단한 방법은 ```Date()``` 함수를 사용하는 것입니다. 이 함수는 현재 날짜와 시간을 반환합니다. 예를 들어, ```Date() > Date()```은 현재 시간보다 미래인지 여부를 확인할 수 있습니다.

### 코드 예시:

```Kotlin
// 현재 날짜와 시간을 출력합니다.
val now = Date()
println("현재 날짜와 시간: $now")

// 비교할 날짜를 생성합니다.
val futureDate = Date(2021, 9, 1)
val pastDate = Date(2021, 7, 1)

// 두 날짜를 비교하여 결과를 출력합니다.
if (futureDate > pastDate) {
    println("$futureDate은 $pastDate보다 미래입니다.")
} else {
    println("$pastDate은 $futureDate보다 미래입니다.")
}
```

### 예상 출력:

```
현재 날짜와 시간: Sat Sep 18 12:00:00 KST 2021
2021년 9월 1일은 2021년 7월 1일보다 미래입니다.
```

## 깊이 파보기:

### 역사적 배경:

날짜를 비교하는 기능은 컴퓨터 과학 분야에서 오랜 역사를 가지고 있습니다. 최초의 컴퓨터는 수치 연산에만 사용되었기 때문에 날짜는 정보 시스템에서 중요한 역할을 담당했습니다. 이 때문에 날짜 비교는 매우 중요한 기능이었고, 개발자들은 이를 쉽고 간단하게 구현하고자 노력하였습니다.

### 대안:

날짜를 비교하는 또 다른 방법은 ```Calender``` 클래스를 사용하는 것입니다. 이 클래스는 날짜를 관리하는 기능뿐만 아니라 여러 가지 다른 기능도 제공하므로 더 많은 유연성을 제공합니다.

### 구현 세부 정보:

날짜를 비교하는 함수는 보통 두 개의 날짜를 매개 변수로 받아 각각의 년, 월, 일을 비교한 후 이를 계산하여 결과를 반환합니다. 이 기능을 구현할 때는 다른 프로그래밍 언어에서도 우리가 사용하는 코드와 비슷하게 작동하므로 어렵지 않게 구현할 수 있습니다.

## 관련 자료:

- [Kotlin Date API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/index.html)
- [Calendar 클래스 사용법](https://developer.android.com/reference/java/util/Calendar)