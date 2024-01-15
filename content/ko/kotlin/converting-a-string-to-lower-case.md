---
title:                "문자열 소문자로 변환하기"
html_title:           "Kotlin: 문자열 소문자로 변환하기"
simple_title:         "문자열 소문자로 변환하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜 필요한가요?

문자열을 소문자로 변환하는 이유는 다양합니다. 사용자로부터 입력 받은 값을 일관된 형식으로 처리하기 위해, 데이터를 비교할 때 대소문자 구분이 필요 없을 때, 또는 문자열이 대/소문자를 혼용하는 경우 등 다양한 상황에서 사용될 수 있습니다.

## 변환하는 방법은?

```Kotlin
val word = "Hello Kotlin"
val lowerCase = word.toLowerCase()

println(lowerCase) // output: hello kotlin
```

만약 사용자로부터 입력받은 값이 숫자나 기호를 포함하고 있어 변환할 수 없는 경우, 숫자나 기호를 제외한 문자열만 소문자로 변환됩니다.

```Kotlin
val sentence = "I love 2021 so much!"
val lowerCase = sentence.toLowerCase()

println(lowerCase) // output: i love so much!
```

## 깊게 들어가보면?

문자열을 소문자로 변환하는 메소드는 매우 간단합니다. 그렇기 때문에 대부분의 프로그래머들은 이를 자주 사용하고 있습니다. 하지만 실제로 문자열을 소문자로 변환하는 과정은 조금 더 복잡합니다. 이 과정에서 ASCII 코드, 유니코드 등과 같은 다양한 인코딩 방식이 사용되며, 이를 이해하고 활용하는 것이 중요합니다.

## 관련 정보는?

- [Kotlin Strings](https://kotlinlang.org/docs/strings.html)
- [Kotlin Reference: toLowerCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Java String toLowerCase()](https://www.baeldung.com/java-string-to-lowercase)