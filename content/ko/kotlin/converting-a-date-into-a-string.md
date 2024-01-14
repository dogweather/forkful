---
title:                "Kotlin: 날짜를 문자열로 변환하기"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜
날짜를 문자열로 변환하는 방법을 배우는 이유는 단순합니다. 프로그래밍에서 날짜는 중요한 부분이며, 이를 문자열로 변환하여 사용하면 보다 편리하게 관리할 수 있기 때문입니다.

## 방법
"```Kotlin
val date = LocalDate.now() // 현재 날짜 가져오기
val dateString = date.toString() // 날짜를 문자열로 변환
println(dateString) // 문자열로 변환된 날짜 출력
```"

위와 같이 코드를 작성하면, 간단하게 날짜를 문자열로 변환할 수 있습니다. 이 외에도 많은 방법들이 있으니, 자신에게 편한 방식을 적용하여 사용하면 됩니다.

## 깊게 들어가기
날짜를 문자열로 변환하는 방법은 다양한 포맷과 함수들이 존재합니다. 예를 들어, 날짜를 "년-월-일" 형식으로 출력하고 싶다면 `date.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))`와 같이 작성할 수 있습니다. 또한, 날짜와 시간을 함께 출력하고 싶다면 `DateTimeFormatter.ISO_DATE_TIME`을 사용하면 됩니다. 이와 같이 날짜를 문자열로 변환하는 방법은 매우 유용하므로, 다양한 방법들을 익히고 활용하는 것이 좋습니다.

## 관련 정보
### 비슷한 주제의 다른 블로그 포스트들:
- [Java에서 날짜를 문자열로 변환하는 방법 알아보기](https://okky.kr/article/467020)
- [Kotlin으로 주소 문자열 파싱하기](http://kotlinlang.org/docs/tutorials/properties.html#properties-declarations)
- [Kotlin 공식 문서 - 날짜와 시간 처리하기](https://kotlinlang.org/docs/reference/datetime.html)