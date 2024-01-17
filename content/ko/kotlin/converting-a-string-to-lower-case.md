---
title:                "문자열을 소문자로 변환하기"
html_title:           "Kotlin: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

자바, 파이썬, 스위프트 등 다양한 프로그래밍 언어에서 문자열을 다루는 경우 문자열을 소문자로 변환하는 경우가 많습니다. 이번 글에서는 코틀린에서 문자열을 소문자로 변환하는 방법에 대해 알아보겠습니다. 이 기능은 대소문자를 구분하지 않고 문자열을 처리해야 할 때 특히 유용합니다.

## 무엇인가요? 그리고 왜 사용하나요?
문자열을 소문자로 변환하는 것은 해당 문자열의 대소문자를 잘 구분하지 않고 문자열을 다루어야 할 때 유용합니다. 예를 들어 사용자의 입력 값을 검증하거나 데이터베이스에서 문자열을 검색할 때, 대소문자를 구분하지 않게 됩니다. 또한, 특정한 지역에서는 대소문자를 구분하지 않는 경우도 있습니다. 이러한 경우 문자열을 소문자로 변환하면 일관성있게 문자열을 다룰 수 있습니다.

## 어떻게 하나요?
코틀린에서 문자열을 소문자로 변환하는 방법은 간단합니다. 아래 코드를 참고하세요.

```Kotlin
val str = "Hello, World!"
val lowerStr = str.toLowerCase()
println(lowerStr)
```

출력 결과는 다음과 같습니다.

```Kotlin
hello, world!
```

## 딥 다이브
문자열을 소문자로 변환하는 기능은 처음부터 존재하지는 않았습니다. 옛날에는 문자열을 모두 대문자로 바꾸어 비교하는 방식을 사용했습니다. 하지만 이는 문자열의 길이가 길어질수록 비교하는데 오래 걸리고, 메모리를 많이 사용하게 되는 단점이 있습니다. 이러한 단점을 극복하기 위해 소문자로 변환하는 방식이 나오게 되었습니다. 또한, 문자열을 소문자로 변환하기 위해 다양한 방식이 존재합니다. 위의 예시 코드는 가장 기본적인 방식이며, 더욱 정교한 방식으로 변환할 수도 있습니다. 하지만 일반적인 상황에서는 위의 예시 코드만으로도 충분합니다.

## 참고 자료
- [Kotlin 문서](https://kotlinlang.org/docs/reference/strings.html#string-case-conversion)