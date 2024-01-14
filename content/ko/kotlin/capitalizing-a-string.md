---
title:    "Kotlin: 문자열 대문자로 만드는 방법"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜
  
문자열을 대문자로 바꾸는 것에 대해 왜 누군가가 참여할까요? 이 기능은 문자열을 가공하거나 데이터를 비교할 때 유용합니다.

## 방법

먼저, Kotlin에서 제공하는 `toUpperCase()` 함수를 사용하여 문자열을 대문자로 변환할 수 있습니다. 다음은 코드 예시와 함께 출력 결과입니다.

```Kotlin
val name = "kotlin programming"
val nameInCaps = name.toUpperCase()
print(nameInCaps)
```

출력 결과:
KOTLIN PROGRAMMING

## 심층 분석

반복적으로 사용할 수 있는 대문자 변환 기능을 작성하는 과정에서 다양한 방식을 배울 수 있습니다. 예를 들어, 문자열을 순회하면서 각 문자를 대문자로 변환하는 방법이 있습니다. 또한, 유니코드에서 제공하는 `toUpperCase()` 함수를 사용하여 지원되지 않는 언어의 문자를 대문자로 변환하는 방법도 있습니다.

## 관련 링크
- [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin String API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/#functions)
- [자바로 문자열 대문자로 변환하는 방법](https://www.javatpoint.com/uppercase-in-java)