---
title:                "Kotlin: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 것이 중요한 이유는 문자열을 다루는 프로그래밍에서 매우 유용하기 때문입니다. 대소문자를 구분하지 않는 검색 기능이나 문자열 비교를 위해 문자열을 소문자로 변환할 수 있습니다.

## 방법
문자열을 소문자로 변환하는 것은 Kotlin에서 쉽게 할 수 있습니다. 우선 입력 문자열을 변수에 할당하고, 그 변수를 다음과 같이 ```.toLowerCase()``` 함수를 이용하여 소문자로 변환할 수 있습니다.

```Kotlin
var input = "Kotlin Programming"
println(input.toLowerCase())
```

출력 결과는 ```kotlin programming```이 됩니다.
만약, 대문자로 변환하고 싶다면 ```.toUpperCase()``` 함수를 사용하면 됩니다.

## 심층 탐구
문자열을 소문자로 변환하는 과정은 문자열을 컴퓨터가 인식하는 ASCII 값으로 변환하는 것입니다. 각각의 문자는 고유의 ASCII 값이 있기 때문에, 소문자 a는 97, 대문자 A는 65입니다. 따라서, ```.toLowerCase()``` 함수는 입력 문자열의 각 문자의 ASCII 값을 확인한 후, 대문자인 경우 32를 뺀 값으로 변환합니다. 즉, 대문자 A는 ASCII 값이 65이기 때문에 32를 뺀 33이 소문자 a의 ASCII 값이기 때문에 소문자 a로 변환하는 것입니다.

## 더 알아보기
소문자로 변환하는 것 외에도 Kotlin에서는 많은 문자열 조작 함수를 제공합니다. [Kotlin 공식 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/index.html)에서 다양한 함수들을 확인해보세요!

## 관련 링크
- [Java vs. Kotlin: String Comparison](https://www.baeldung.com/java-vs-kotlin-string-comparison)
- [Converting Strings to Lowercase and Uppercase in Kotlin](https://www.tutorialkart.com/kotlin/convert-string-to-lowercase-uppercase-kotlin/)