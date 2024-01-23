---
title:                "문자열 대문자로 변환하기"
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
문자열의 첫 글자나 모든 단어의 첫 글자를 대문자로 만드는 것을 'capitalizing a string'이라고 합니다. 사용자 인터페이스를 깔끔하게 보이게 하거나 문서를 표준 형식에 맞추기 위해 프로그래머들이 사용합니다.

## How to: (방법)
Kotlin에서 문자열을 대문자로 만드는 방법을 봅시다. 먼저, 단어의 첫 글자를 대문자로 만드는 `capitalize()` 함수부터 시작하겠습니다. (주의: Kotlin 1.5부터는 `replaceFirstChar`로 대체되었습니다.)

```kotlin
fun main() {
    val greeting = "hello, world!"
    val capitalizedGreeting = greeting.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }
    println(capitalizedGreeting) // Hello, world!
}
```

모든 단어의 첫 글자를 대문자로 만들려면 다음과 같이 확장 함수를 사용할 수 있습니다.

```kotlin
fun String.capitalizeWords(): String = this.split(" ").joinToString(" ") { it.capitalize() }

fun main() {
    val title = "the lord of the rings"
    val capitalizedTitle = title.capitalizeWords()
    println(capitalizedTitle) // The Lord Of The Rings
}
```

## Deep Dive (더 깊이 알아보기)
문자열을 대문자로 만드는 것은 프로그래밍 언어마다 다르게 처리됩니다. 과거 Kotlin에서는 `capitalize()` 함수 하나로 간단히 처리했습니다. 하지만 Kotlin 1.5 이후 `capitalize()`는 deprecated 되었고, `replaceFirstChar`와 `titlecase`를 조합하여 사용해야 합니다. 다른 언어에서는, 예를 들어 Python에서는 `title()`이라는 독립된 내장 함수를 제공합니다.

대문자 사용은 로케일에 따라 결과가 달라질 수 있음을 염두에 두어야 합니다. 예를 들어 특정 언어에서는 특정 문자가 대문자 형태가 없거나 다른 문자로 변환될 수 있습니다.

대안적으로, 문자열을 전부 대문자나 소문자로 만드는 `toUpperCase()`나 `toLowerCase()` 함수도 있으며, 이들은 또한 로케일을 파라미터로 받을 수도 있습니다.

## See Also (더 보기)
- Kotlin 공식 문서의 [문자열 함수](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)
- [capitalize() 함수 deprecation에 대한 Kotlin 공식 블로그](https://blog.jetbrains.com/kotlin/2021/05/kotlin-1-5-0-released/)
- [Kotlin 표준 라이브러리의 String 클래스](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
