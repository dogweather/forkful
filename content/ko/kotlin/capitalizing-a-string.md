---
title:                "문자열의 대문자 변환"
html_title:           "Kotlin: 문자열의 대문자 변환"
simple_title:         "문자열의 대문자 변환"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

왜: 문자열의 대문자로 변환하는 것이 왜 필요한지에 대해 최대 2 문장으로 설명합니다.

## 왜

문자열을 대문자로 변환하는 것은 일반적으로 프로그래밍에서 사용자 입력을 표준화하거나 문자열을 비교할 때 유용합니다. 또한 대문자로 변환된 문자열은 더 깔끔하고 일관성 있게 보일 수 있습니다.

## Kotlin으로 대문자로 변환하기

문자열을 대문자로 변환하는 방법에는 여러 가지가 있지만, 문자열을 다루는 다양한 함수와 연산자를 제공하는 Kotlin을 사용하는 것이 가장 효율적입니다.

```Kotlin
var str = "hello world"
println(str.capitalize()) // 출력 결과: Hello world
println(str.toUpperCase()) // 출력 결과: HELLO WORLD
```

위의 코드에서 `capitalize()` 함수는 첫 번째 문자를 대문자로 변환하고 나머지 문자는 소문자로 변환합니다. `toUpperCase()` 함수는 문자열의 모든 문자를 대문자로 변환합니다.

## 깊이 알아보기

Kotlin은 문자열을 다루는 데 유용한 여러 가지 함수와 연산자를 제공합니다. 예를 들어, `replace()` 함수를 사용하면 문자열 내의 특정 부분을 다른 문자로 바꿀 수 있습니다.

```Kotlin
val str = "kotlin is fun!"
println(str.replace("fun", "awesome")) // 출력 결과: kotlin is awesome!
```

또한 `substring()` 함수를 사용하여 문자열의 일부분만 추출할 수 있습니다. 이 함수는 시작 인덱스와 끝 인덱스를 매개변수로 받아 해당 범위 내의 문자열을 반환합니다.

```Kotlin
val str = "hello"
println(str.substring(1, 3)) // 출력 결과: el
```

## 참고 자료

- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [String Manipulation in Kotlin](https://www.baeldung.com/kotlin/string-manipulation)