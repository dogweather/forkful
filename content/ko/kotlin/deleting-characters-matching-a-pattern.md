---
title:                "Kotlin: 패턴과 일치하는 문자 삭제하기"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜
코틀린 언어로 문자열 처리를 할 때, 특정한 패턴과 일치하는 문자를 삭제하는 것은 자주 사용되는 작업입니다.

## 방법
코틀린에서는 `replace(Regex, String)` 함수를 사용하여 패턴에 일치하는 문자를 삭제할 수 있습니다. 예를 들어, 아래의 예시 코드는 `hello123` 문자열에서 숫자를 삭제하는 코드입니다.

```Kotlin
val input = "hello123"
val regex = Regex("[0-9]")
val output = input.replace(regex, "")
println(output)

// Output: hello
```

위의 코드에서는 `replace` 함수를 사용하여 새로운 문자열을 생성했지만, 원본 문자열을 변경하고 싶다면 `replace(Regex, replacement)` 를 사용할 수도 있습니다.

```Kotlin
var input = "hello123"
val regex = Regex("[0-9]")
val output = input.replace(regex) { "" }
println(input)

// Output: hello
```

위의 예시에서는 문자열이 변경되었음을 확인할 수 있습니다.

## 딥 다이브
문자열에서 패턴에 일치하는 문자를 삭제하는 방법 외에도 `replace` 함수를 사용하여 다양한 형태의 문자열 처리를 할 수 있습니다. 예를 들어, 정규식 패턴을 이용하여 특정 문자를 다른 문자로 치환하는 것도 가능합니다. 더 많은 예시와 살펴볼 수 있는 다른 유용한 문자열 처리 함수들이 있으니, 코틀린 공식 문서를 참조하시기 바랍니다.

## 참고
- [코틀린 공식 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [정규식 패턴 작성 가이드](https://www.regular-expressions.info/quickstart.html)
- [코틀린 정규식 예시 배우기](https://www.baeldung.com/kotlin-regex)