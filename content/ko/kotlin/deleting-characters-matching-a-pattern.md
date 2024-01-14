---
title:                "Kotlin: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜

Kotlin에서 문자열의 패턴과 일치하는 문자를 삭제하는 것이 유용한 이유는 특정 문자열이나 패턴을 제거해야 할 때 시간을 절약하고 불필요한 과정을 줄일 수 있다는 점입니다.

## 어떻게

```Kotlin
val str = "Hello, World!"
val newStr = str.replace(Regex("[aeiou]"), "") // "Hll, Wrld!"

val longStr = "This is a long string with multiple occurrences of the letter e."
val newLongStr = longStr.replace(Regex("[e]"), "") // "This is a long string with multipl occurrences of th lttr ."
```

## 깊이 알아보기

문자열에서 특정 패턴을 찾고 대체하여 문자를 삭제하는 것은 `Regex` 클래스를 이용하여 가능합니다. `replace()` 메서드는 문자열의 정규표현식과 일치하는 모든 부분을 대체하여 새로운 문자열을 반환합니다.

## 더 많은 정보

- [Kotlin의 정규표현식 사용 방법](https://codechacha.com/ko/kotlin-regular-expression/)
- [Kotlin 문자열 다루기](https://kotlinlang.org/docs/strings.html)
- [Regex 클래스 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/) 

# 더 알아보기

다음은 Kotlin에서 문자열을 다루는 다른 기능과 관련된 유용한 링크들입니다.

## 링크

- [Kotlin 튜토리얼](https://kotlinlang.org/docs/tutorials/)
- [코틀린 코딩 컨벤션](https://kotlinlang.org/docs/coding-conventions.html)
- [Kotlin 단축키](https://kotlinlang.org/docs/tutorials/command-line.html#ide-shortcuts)
- [Kotlin 표준 라이브러리](https://kotlinlang.org/api/latest/jvm/stdlib/)