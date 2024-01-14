---
title:                "Kotlin: 하위 문자열 추출하기"
simple_title:         "하위 문자열 추출하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 서브스트링을 추출하는 것은 다양한 이유가 있을 수 있습니다. 예를 들어, 특정 문자열에서 원하는 부분만 따로 사용하고 싶을 때나 데이터 처리 과정에서 필요한 문자열만 추출하고 싶을 때가 있을 수 있습니다.

## 추출하는 방법

```Kotlin
val originalString = "안녕하세요! Kotlin 프로그래밍을 배워봅시다."
val subString = originalString.substring(3, 7)
println(subString)
```
> 출력 결과: 녕하세

위의 코드에서 `substring()` 함수를 사용하여 원하는 문자열을 추출할 수 있습니다. 파라미터에 시작 인덱스와 끝 인덱스를 넣어서 추출할 부분을 지정할 수 있습니다. 또한, `substringBefore()`와 `substringAfter()` 함수를 사용하여 특정 문자열 이전이나 이후의 문자열을 추출할 수도 있습니다.

## 더 깊게 들어가기

서브스트링을 추출하는 방법에는 여러 가지 종류가 있습니다. 예를 들어, 정규표현식을 사용하여 원하는 패턴의 문자열만 추출할 수 있고, `split()` 함수를 사용하여 문자열을 나눠서 추출할 수도 있습니다. 더 많은 예제와 함수 사용법은 [Kotlin 공식 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/index.html)를 참고해주세요.

## 관련 자료

[Kotlin Text Processing](https://developer.android.com/reference/kotlin/text/index.html)

[Learn Kotlin in This Overview](https://kotlinlang.org/docs/reference/)