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

# 왜

날짜를 문자열로 변환하는 작업은 소프트웨어 개발에서 매우 자주 사용됩니다. 구체적으로는 데이터와 상호작용하거나 사용자 인터페이스에서 날짜를 표시하는데 사용되는 경우가 많습니다.

# 사용 방법

날짜를 문자열로 바꾸는 방법은 Kotlin에서 지원하는 여러 가지 방법이 있습니다.

## ```Kotlin
val date = LocalDate.of(2021, 10, 20)
val dateString = date.format(DateTimeFormatter.ofPattern("yyyy년 MM월 dd일"))
println(dateString) 
```

위의 예제에서는 먼저 원하는 날짜를 LocalDate 객체로 생성합니다. 그리고 해당 객체를 format() 메서드를 사용하여 지정된 패턴으로 문자열로 변환합니다. 문자열의 출력은 '2021년 10월 20일'과 같이 나오게 됩니다.

또는 다음과 같이 간단하게도 사용할 수 있습니다.

## ```Kotlin
val today = LocalDate.now()
val dateString = today.toString() 
println(dateString) 
```

위의 예제에서는 현재 날짜를 LocalDate 객체로 생성하고, toString() 메서드를 사용하여 기본 패턴으로 날짜를 문자열로 출력합니다. 출력은 '2021-05-01'과 같이 기본적인 형식으로 나오게 됩니다.

# 깊이 파고들기

Kotlin에서 날짜를 문자열로 변환하는 작업은 자바의 SimpleDateFormat 클래스를 사용하는 것과 유사합니다. 하지만 더 간단하고 안전하며 기능적인 방식으로 제공됩니다.

또한 Kotlin에서는 문자열이나 숫자 등 다른 데이터 타입과 날짜 간의 변환이 자유롭게 가능합니다. 예를 들어, 위 코드에서 date.format() 대신 date.toString()으로 호출해도 같은 결과를 얻을 수 있습니다. 또한 포맷팅 패턴의 기호도 자유롭게 조합하여 사용할 수 있습니다.

# 관련 항목

- [Kotlin 날짜 처리 공식 문서](https://kotlinlang.org/docs/datetime.html)
- [Kotlin 날짜와 시간 라이브러리 참고 문서](https://docs.oracle.com/javase/tutorial/datetime/iso/datetime.html)