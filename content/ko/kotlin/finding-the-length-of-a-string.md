---
title:                "Kotlin: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 구하는 것이 왜 중요한지 궁금하신가요? 문자열은 프로그래밍에서 매우 중요한 개념이며, 긴 문자열을 다룰 때 유용하게 활용될 수 있습니다. 문자열의 길이를 알아내는 것은 프로그래밍에서 많은 상황에서 필요한 작업입니다.

## 어떻게 해야 할까요?

우선, 문자열을 저장하는 변수를 만들고 이 변수에 원하는 문자열을 할당합니다. 그리고 아래의 코드를 통해 문자열의 길이를 구할 수 있습니다.

```Kotlin
var str = "안녕하세요"
println(str.length) // 출력 결과: 5
```

만약 문자열의 길이가 0일 경우에는 빈 문자열이라고 출력됩니다. 또는 이전 버전의 자바에서 사용했던 `length()` 메소드를 사용할 수도 있습니다.

```Kotlin
var str = "Hello World"
println(str.length()) // 출력 결과: 11
```

## 딥 다이브

때로는 문자열의 길이를 구하는 것만으로는 충분하지 않을 수 있습니다. 예를 들어, 공백 문자를 포함한 경우에는 앞서 소개한 방법으로는 제대로된 결과를 얻을 수 없습니다. 이런 경우에는 `trim()` 메소드를 사용하여 공백 문자를 제거한 뒤에 길이를 구해야 합니다. 또는 다른 언어에서도 문자열의 길이를 구하는 방법이 존재할 수 있으니, 해당 언어의 문서를 참조하는 것이 좋습니다.

## 참고 자료

- [Kotlin 문자열 API 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html)
- [Java에서 문자열 길이 구하는 방법](https://www.tutorialspoint.com/java/java_string_length.htm)
- [Java String vs Kotlin String](https://kotlinlang.org/docs/reference/basic-types.html#strings)