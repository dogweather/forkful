---
title:    "Kotlin: 문자열을 소문자로 변환하기"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

자바나 코틀린에서는 문자열을 처리할 때 대문자와 소문자를 따로 구분합니다. 때로는 특정한 이유로 문자열을 모두 소문자로 변환해야 할 때가 있습니다. 이 포스트에서는 코틀린에서 문자열을 소문자로 변환하는 방법에 대해 알아보겠습니다.

## 방법

문자열을 소문자로 변환하는 방법은 코틀린에서 제공하는 `toLowerCase()` 함수를 사용하는 것입니다. 이 함수는 모든 문자를 소문자로 변환해줍니다. 예를 들어, 다음과 같이 문자열을 정의하고 `toLowerCase()` 함수를 호출하면:

```Kotlin
val str = "Hello World!"
val lowerCaseStr = str.toLowerCase()
```

결과는 `hello world!`가 됩니다. 또한, 만약 문자열에 대한 참조가 없는 경우에는 함수를 바로 호출할 수 있습니다:

```Kotlin
val lowerCaseStr = "Hello World!".toLowerCase()
```

이렇게 하면 작성하는 코드가 간결해지고 편리해집니다.

## Deep Dive

`toLowerCase()` 함수는 문자열을 인수로 받아 새로운 문자열을 반환하는 무시합니다. 따라서 원본 문자열을 변경하지 않고 변환된 문자열을 사용할 수 있습니다. 이는 원본 문자열을 재사용해야 할 때 유용합니다.

또한, 이 함수는 코틀린에서 자동으로 나타나는 유니코드 변환을 지원합니다. 따라서 여러 언어로 된 문자열도 소문자로 변환할 수 있습니다.

이 함수는 문자열의 모든 문자를 소문자로 변환하기 때문에, 일부 특수한 상황에서 원하는 결과가 나오지 않을 수 있습니다. 이럴 때에는 정규표현식 등 다른 방법을 사용해야 할 수도 있습니다.

## See Also

- [코틀린 문자열 관련 공식 문서](https://kotlinlang.org/docs/reference/strings.html)
- [코틀린 정규표현식 관련 공식 문서](https://kotlinlang.org/docs/reference/regular-expressions.html)
- [코틀린 표준 라이브러리 공식 문서](https://kotlinlang.org/api/latest/jvm/stdlib/index.html)