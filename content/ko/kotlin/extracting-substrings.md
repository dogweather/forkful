---
title:    "Kotlin: 부분 문자열 추출하기"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## 왜

서브스트링을 추출하는 이유는 문자열을 다룰 때 매우 유용한 기능입니다. 때때로 문자열의 특정 부분만 사용해야 할 때가 있으며, 서브스트링을 추출하는 것은 이를 간단하게 만들어줍니다.

## 하는 법

```Kotlin
val str = "안녕하세요. 코틀린 블로그입니다."

// 첫 번째 문자부터 다섯 번째 문자까지 추출
val substring1 = str.substring(0, 5)
println(substring1)

// 빈 칸을 기준으로 문자열 나누기
val words = str.split(" ")
println(words)
```

출력:

```
안녕하세요.
[안녕하세요., 코틀린, 블로그입니다.]
```

위의 예제에서는 `substring()` 함수를 사용하여 문자열의 첫 번째부터 다섯 번째까지의 문자를 추출하고, `split()` 함수를 사용하여 문자열을 빈 칸을 기준으로 나눕니다. 이 외에도 `indexOf()` 함수를 사용하여 특정 문자열의 위치를 찾아낼 수 있습니다.

## 더 들어가기

서브스트링을 추출하는 방법에 대해 더 깊이 알아보겠습니다. 서브스트링을 추출할 때 `substring()` 함수 외에도 `slice()` 함수를 사용할 수 있습니다. `slice()` 함수를 사용하면 여러 개의 인덱스를 지정하여 한 번에 여러 개의 서브스트링을 추출할 수 있습니다. 또한, 정규식을 사용하여 추출할 문자열의 패턴을 지정할 수도 있습니다.

## 더 보기

서브스트링 추출에 대해 더 많은 정보를 알고 싶다면 아래의 링크를 참고해보세요.

- [Kotlin 문자열 관련 함수](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/)
- [Kotlin 정규식 사용법](https://junsday.net/21)
- [Kotlin 공식 문서](https://kotlinlang.org/docs/home.html)

## 참고자료

- [Kotlin 문자열 다루기](https://codechacha.com/ko/kotlin-string-manipulation/)
- [Kotlin 정규식 사용법](https://unclosed.org/tips/how-to-use-regular-expression-regex-in-kotlin/)
- [Kotlin 공식 문서](https://kotlinlang.org/docs/home.html)