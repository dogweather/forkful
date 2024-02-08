---
title:                "패턴에 일치하는 문자 삭제"
aliases:
- ko/kotlin/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:43.377517-07:00
model:                 gpt-4-1106-preview
simple_title:         "패턴에 일치하는 문자 삭제"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 특정 패턴에 일치하는 문자를 삭제하는 것은 데이터를 정제하고 원하는 형식으로 가공하기 위해 사용됩니다. 프로그래머들은 불필요한 데이터를 제거하거나 입력값을 표준화하기 위해 이 작업을 수행합니다.

## How to: (어떻게:)
```kotlin
fun main() {
    val originalString = "안녕하세요! 123 이 커뮤니티는 코틀린을 사랑합니다."
    val pattern = "\\d+".toRegex() // 숫자에 일치하는 패턴
    val cleanedString = originalString.replace(pattern, "") // 패턴에 일치하는 문자 삭제

    println(cleanedString) // "안녕하세요!  이 커뮤니티는 코틀린을 사랑합니다."
}
```

## Deep Dive (심화 탐구)
문자 삭제는 초기 프로그래밍 시절부터 데이터 처리의 일부였습니다. 정규 표현식(Regular Expressions)은 1950년대로 거슬러 올라가며, 문자열 처리에 강력한 도구입니다.

코틀린에서는 `replace` 함수로 간단히 문자를 삭제할 수 있으며, 정규 표현식을 사용해 더 넓은 범위의 패턴을 대응할 수 있습니다. `replace` 말고도, `filterNot` 같은 함수를 사용해 문자를 제거하는 방법도 있습니다.

```kotlin
val stringWithoutDigits = originalString.filterNot { it.isDigit() }
```

여기서는 `isDigit()` 함수를 사용해 숫자가 아닌 문자만을 남긴 것입니다. 정규표현식이나 문자열 함수 등 상황에 맞는 다양한 방법을 선택할 수 있습니다.

## See Also (참고자료)
- Kotlin 공식 문서: [문자열 처리](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)
- 정규 표현식에 관한 자세한 내용: [regular-expressions.info](https://www.regular-expressions.info/)
- `filterNot` 함수 사용 예: [Kotlinlang - filterNot](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/filter-not.html)
