---
title:                "Kotlin: 부분 문자열 추출"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜 
문자열의 일부를 추출하는 것은 프로그램에서 매우 유용한 기술입니다. 예를 들어, 사용자로부터 입력받은 주민등록번호에서 생년월일을 추출하여 처리하는 경우 등 여러 상황에서 필요할 수 있습니다.

## 추출 방법
우선, 문자열을 다음과 같은 형식으로 정의하겠습니다.
``Kotlin
val input: String = "123456-1234567"
``
먼저, .substring() 메소드를 사용하여 인덱스를 지정하여 원하는 부분을 추출할 수 있습니다. 예를 들어, 위의 입력값에서 첫 번째 숫자인 "1"을 추출하려면 다음과 같은 코드를 사용할 수 있습니다.
``Kotlin
val firstNumber = input.substring(0, 1)
println(firstNumber) // 출력 결과: 1
``
또 다른 방법으로는 .slice() 메소드를 사용하여 문자열에 대한 인덱스를 리스트로 전달하는 방법이 있습니다. 예를 들어, 위의 입력값에서 두 자리의 숫자 "12"를 추출하려면 다음과 같이 사용할 수 있습니다. 
``Kotlin
val twoDigits = input.slice(0..1)
println(twoDigits) // 출력 결과: 12
``
위의 예시 코드에서 0..1은 0부터 1까지의 인덱스를 나타내는 리스트입니다.

## 깊이 있게 알아보기
. substring() 메소드와 .slice() 메소드 외에도 다양한 방법으로 문자열에서 일부를 추출할 수 있습니다. 이 글에서는 기본적인 방법만 소개하고 있지만, Kotlin에서 제공하는 다양한 문자열 관련 메소드를 찾아보며 적절한 방법을 선택하시면 됩니다. 또한, 인덱스를 제대로 지정하는 것이 중요하니 주의해야 합니다. 

## 참고 자료
- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin String Methods](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)
- [Kotlin .substring() 메소드 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring.html)
- [Kotlin .slice() 메소드 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/slice.html)

참고 자료를 통해 더욱 깊이 있는 공부를 할 수 있으니, 관심있는 분들은 링크를 확인해보시기 바랍니다.

## 더 알아보기 
.substring()과 .slice() 메소드 이외에도 Kotlin에서 제공하는 다양한 문자열 관련 메소드를 살펴보았습니다. 이 글을 통해 여러분도 문자열에서 원하는 부분을 쉽게 추출할 수 있게 되었기를 바랍니다.