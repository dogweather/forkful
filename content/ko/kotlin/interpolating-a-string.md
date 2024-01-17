---
title:                "문자열 보간"
html_title:           "Kotlin: 문자열 보간"
simple_title:         "문자열 보간"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Kotlin에서 문자열 보간하기 

## 무엇이며 왜?
문자열 보간하기란 변수 값이나 식을 문자열에 쉽게 삽입하는 것을 말합니다. 이를 프로그래머들은 간단한 문장을 만드는데 사용합니다. 예를 들어, 사용자의 이름이나 나이를 출력할 때 유용하게 사용할 수 있습니다.

## 하는 방법:
```
Kotlin
val name = "John"
println("My name is $name") // Output: My name is John
```
```
Kotlin
val num1 = 10
val num2 = 20
println("The sum of $num1 and $num2 is ${num1 + num2}") // Output: The sum of 10 and 20 is 30
```

## 깊이 들어가기:
- 문자열 보간은 코틀린 2.0 버전에서 처음 도입되었습니다.
- 다른 방법으로는  문자열 템플릿 사용이 있습니다. 하지만 문자열 보간은 변수나 식을 더 간편하게 삽입할 수 있도록 해줍니다.
- 실제로는 변수나 식이 문자열로 변환되는 과정을 통해 동작합니다.

## 참고자료:
- [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/basic-types.html#string-interpolation)
- [Kotlin 코딩 패턴](https://kotlinlang.org/docs/reference/coding-conventions.html#string-interpolation)