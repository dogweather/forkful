---
date: 2024-01-20 17:42:43.377517-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) \uBB38\uC790 \uC0AD\uC81C\uB294 \uCD08\
  \uAE30 \uD504\uB85C\uADF8\uB798\uBC0D \uC2DC\uC808\uBD80\uD130 \uB370\uC774\uD130\
  \ \uCC98\uB9AC\uC758 \uC77C\uBD80\uC600\uC2B5\uB2C8\uB2E4. \uC815\uADDC \uD45C\uD604\
  \uC2DD(Regular Expressions)\uC740 1950\uB144\uB300\uB85C \uAC70\uC2AC\uB7EC \uC62C\
  \uB77C\uAC00\uBA70, \uBB38\uC790\uC5F4 \uCC98\uB9AC\uC5D0 \uAC15\uB825\uD55C \uB3C4\
  \uAD6C\uC785\uB2C8\uB2E4. \uCF54\uD2C0\uB9B0\uC5D0\uC11C\uB294 `replace` \uD568\uC218\
  \uB85C \uAC04\uB2E8\uD788 \uBB38\uC790\uB97C \uC0AD\uC81C\uD560 \uC218\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.896193-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:) \uBB38\uC790 \uC0AD\uC81C\uB294 \uCD08\uAE30 \uD504\
  \uB85C\uADF8\uB798\uBC0D \uC2DC\uC808\uBD80\uD130 \uB370\uC774\uD130 \uCC98\uB9AC\
  \uC758 \uC77C\uBD80\uC600\uC2B5\uB2C8\uB2E4."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
weight: 5
---

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
