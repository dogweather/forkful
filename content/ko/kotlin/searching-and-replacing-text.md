---
title:    "Kotlin: 텍스트 검색과 교체"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜
문자열을 검색하고 바꾸는 것은 프로그래밍에서 일반적인 작업 중 하나입니다. 인터넷의 무수한 정보들 중에서 우리가 원하는 내용을 찾아서 변경해야 할 때 검색과 바꾸기 기능을 사용할 수 있습니다.

## 하는 법
```Kotlin
val str = "Hello, world!"
val newStr = str.replace("world", "Kotlin")
println(newStr)
```
코틀린에서는 `replace` 함수를 사용하여 특정한 문자열을 다른 문자열로 쉽게 바꿀 수 있습니다. 위의 예시 코드에서는 "Hello, world!"라는 문자열에서 "world"를 "Kotlin"으로 바꾸어 새로운 문자열을 생성하고 출력하도록 작성되었습니다. 따라서 결과는 "Hello, Kotlin!"이 됩니다.

```Kotlin
val str2 = "Kotlin is fun"
val newStr2 = str2.replace("fun", "awesome")
println(newStr2)
```
위의 예시 코드에서는 `replace` 함수를 사용하여 "fun"이라는 문자열을 "awesome"로 바꾸어 새로운 문자열을 생성하고 출력하도록 작성되었습니다. 따라서 결과는 "Kotlin is awesome!"가 됩니다.

## 깊이 파고들기
검색과 바꾸기 기능을 사용할 때 알아두면 유용한 팁들이 있습니다. 예를 들어, `replace` 함수는 첫 번째 매개변수로 일치하는 문자열을 받지만, 정규식을 사용하여 여러 개의 문자열을 동시에 바꿀 수도 있습니다. 또한 `replaceFirst` 함수를 사용하면 첫 번째 일치하는 문자열만 바꿀 수 있습니다.

## 더 알아보기
Markdown 문서의 끝에는 "## 관련 링크" 헤더와 함께 관련된 링크들을 나열해 두겠습니다.

- [코틀린 공식 문서 - 문자열 검색 및 바꾸기](https://kotlinlang.org/docs/tutorials/strings-misc.html#string-indexing-and-slicing)
- [코틀린 문자열 다루기 방법 - Regex](https://codechacha.com/ko/kotlin-string-contains-match-regex/)
- [코틀린 문법 - 정규식](https://codechacha.com/ko/kotlin-regex/)