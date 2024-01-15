---
title:                "Youngsang godeunghaneun wonrak johap cheonyul"
html_title:           "Kotlin: Youngsang godeunghaneun wonrak johap cheonyul"
simple_title:         "Youngsang godeunghaneun wonrak johap cheonyul"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜
누군가가 패턴과 일치하는 문자를 삭제하는 것에 삽입할 이유를 최대 2개의 문장으로 설명합니다.

## 어떻게
코드 블록 " ```Kotlin ...```" 내의 코딩 예제와 샘플 출력을 사용하여 삭제된 문자에 대한 쉬운 설명을 제공합니다.

```Kotlin
val input = "안녕하세요, Kotlin!"
val pattern = "[^가-힣 ]".toRegex()

val output = input.replace(pattern, "")
println(output)
```
출력: 안녕하세요 Kotlin

## 더 깊게 들어가기
패턴과 일치하는 문자를 삭제하는 더 많은 방법에 대해 알아봅니다. 이를테면, 정규식 패턴을 사용하는 것 외에도 문자열 메소드를 사용하여 문자를 삭제하는 방법이 있을 수 있습니다.

## 더 알아보기
관련된 다른 정보와 자료들을 살펴봅니다.

- [코틀린 공식 사이트](https://kotlinlang.org/docs/reference/basic-types.html)
- [정규식 패턴에 대한 더 자세한 설명](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [문자열 메소드의 다른 활용법](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)