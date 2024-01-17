---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Kotlin: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

패턴과 일치하는 문자를 삭제하는 것은 프로그래머들이 특정한 문자를 원하지 않을 때 사용하는 기술입니다. 이를 통해 코드의 가독성을 높이고 원하는 결과를 얻을 수 있습니다.

## 하는 방법:

```Kotlin
val text = "Hello, world!"
println(text.replace(Regex("[aeiou]"), "")) 
```
```
Hll, wrld!
```

첫째 줄에서는 삭제할 문자가 포함된 텍스트를 정의하고 두 번째 줄에서는 `replace` 함수를 사용하여 정규식 패턴 `[aeiou]`과 일치하는 모든 문자를 비워줍니다. 그 결과로 모음이 삭제된 `Hll, wrld!`가 출력됩니다.

## 깊이 파고들기:

(1) 이 기술은 정규식의 활용으로부터 탄생했습니다. 정규식은 특정한 패턴의 문자열을 찾고 변환하는 데 매우 유용합니다. (2) 대체로 문자를 삭제하기 위해서는 다른 방법들도 존재하지만 정규식을 사용하는 것이 더 쉽고 효율적입니다. (3) Kotlin에서 `replace` 함수는 내부적으로 `StringBuilder`를 사용하여 문자열을 조작합니다. 또한 `Regex` 클래스를 사용하여 정규식 패턴을 정의하고 대체할 문자를 지정할 수 있습니다.

## 또한 참고:

- [Kotlin - 정규식 문서](https://kotlinlang.org/docs/reference/regular-expressions.html)
- [정규식 튜토리얼](https://www.tutorialspoint.com/kotlin/kotlin_regular_expressions.htm)
- [Kotlin에서 문자열 다루기](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)